{-
pgxhtml, an XSLT-based PostgreSQL web interface.
Copyright (C) 2018-2019 defanor <defanor@uberspace.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{- |
Description :  XSLT-based PostgreSQL web interface
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

This is a tool to make custom web (HTTP with XHTML) interfaces to
PostgreSQL databases, using XSLT for templating and SQL for querying,
HTTP basic authentication and PostgreSQL roles for authentication.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple.Types as PT
import Database.PostgreSQL.Simple
import Network.Wai
import Network.HTTP.Types as HT
import Database.PostgreSQL.Simple.FromField
  (FromField, fromField, returnError, typename)
import Web.FormUrlEncoded (Form(..), urlDecodeForm)
import Database.PostgreSQL.Simple.ToField (Action (..))
import Database.PostgreSQL.Simple.Internal (withConnection)
import Database.PostgreSQL.LibPQ (getCancel, cancel)
import Data.Maybe (mapMaybe)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import Network.HTTP.Types.Header (hWWWAuthenticate)
import Network.Wai.Middleware.Timeout (timeout)
import Network.Wai.Cli (defWaiMain)
import Control.Monad (join)
import Control.Arrow ((***))
import Control.Exception (bracket, finally, catches, Handler(..))
import System.FilePath (replaceExtension, takeFileName, (</>))
import System.Envy (decodeEnv, FromEnv(..), envMaybe, (.!=))
import Foreign
import Foreign.C


data XmlDoc
data XmlSaveCtxt
data XsltStylesheet
data XsltTransformContext

foreign import ccall "xmlReadMemory" xmlReadMemory ::
  CString -> CInt -> CString -> CString -> CInt -> IO (Ptr XmlDoc)
foreign import ccall "xmlFreeDoc" xmlFreeDoc :: Ptr XmlDoc -> IO ()
foreign import ccall "xsltParseStylesheetFile" xsltParseStylesheetFile ::
  CString -> IO (Ptr XsltStylesheet)
foreign import ccall "xsltFreeStylesheet" xsltFreeStylesheet ::
  Ptr XsltStylesheet -> IO ()
foreign import ccall "xsltNewTransformContext" xsltNewTransformContext ::
  Ptr XsltStylesheet -> Ptr XmlDoc -> IO (Ptr XsltTransformContext)
foreign import ccall "xsltFreeTransformContext" xsltFreeTransformContext ::
  Ptr XsltTransformContext -> IO ()
foreign import ccall "xsltQuoteUserParams" xsltQuoteUserParams ::
  Ptr XsltTransformContext -> Ptr CString -> IO CInt
foreign import ccall "xsltApplyStylesheetUser" xsltApplyStylesheetUser ::
  Ptr XsltStylesheet -> Ptr XmlDoc -> Ptr CString -> CString -> Ptr () ->
  Ptr XsltTransformContext -> IO (Ptr XmlDoc)
foreign import ccall "xsltSaveResultToString" xsltSaveResultToString ::
  Ptr CString -> Ptr CInt -> Ptr XmlDoc -> Ptr XsltStylesheet -> IO CInt
foreign import ccall "exsltRegisterAll" exsltRegisterAll :: IO ()

transform :: BS.ByteString
          -- ^ document
          -> String
          -- ^ base URI
          -> FilePath
          -- ^ path to stylesheet
          -> [(String, String)]
          -- ^ string params
          -> IO BS.ByteString
transform docBS baseStr pathStr stringParams =
  BS.useAsCStringLen docBS $ \(docCStr, docCStrLen) ->
  withCString baseStr $ \baseCStr ->
  withCString pathStr $ \pathCStr ->
  alloca $ \bufPtr ->
  alloca $ \lenPtr ->
  bracket (notNull $ xmlReadMemory docCStr (fromIntegral docCStrLen) baseCStr nullPtr 0)
  xmlFreeDoc $ \doc ->
  bracket (mapM newCString (concatMap (\(x, y) -> [x, y]) stringParams))
  (mapM free) $ \params ->
  withArray0 nullPtr params $ \paramsArr ->
  withArray0 nullPtr [] $ \emptyArr ->
  bracket (notNull $ xsltParseStylesheetFile pathCStr) xsltFreeStylesheet $ \stylesheet ->
  bracket (notNull $ xsltNewTransformContext stylesheet doc) xsltFreeTransformContext $
  \tc -> xsltQuoteUserParams tc paramsArr >>
  (bracket (notNull $ xsltApplyStylesheetUser stylesheet doc emptyArr nullPtr nullPtr tc)
    xmlFreeDoc $ \res ->
      xsltSaveResultToString bufPtr lenPtr res stylesheet >>
      bracket (peek bufPtr) free BS.packCString)
  where
    notNull :: IO (Ptr a) -> IO (Ptr a)
    notNull a = a >>= \p -> if p == nullPtr
                            then error "Unexpected NULL pointer"
                            else pure p
  -- TODO: improve error handling

newtype PgXmlString = PgXmlString { unPgXmlString :: BS.ByteString }

instance FromField PgXmlString where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just mdata) = do
    tName <- typename f
    if tName /= "xml"
      then returnError ConversionFailed f $
           "Expected xml type, got " ++ BS.unpack tName
      else pure $ PgXmlString mdata

data EnvConfig = EnvConfig
  { connectionTimeout :: Int
    -- ^ HTTP connection timeout, in seconds
  , xsltDirectory :: FilePath
    -- ^ A directory to read XSLTs from
  }

instance FromEnv EnvConfig where
  fromEnv =
    EnvConfig <$> envMaybe "TIMEOUT" .!= 10 <*> envMaybe "XSLT_DIR" .!= ""

data Error = ESQL SqlError
           | EFormat FormatError
           | EQuery QueryError
           | EResult ResultError
           | EOther String

formToFields :: Form -> [(T.Text, T.Text)]
formToFields = mapMaybe toField . HM.toList . unForm
  where toField (f, [v]) = Just (f, v)
        toField _ = Nothing

cancelAndClose :: Connection -> IO (Maybe BS.ByteString)
cancelAndClose c = cancelConn `finally` close c
  where cancelConn = withConnection c $ \conn -> do
          cl <- getCancel conn
          case cl of
            Nothing -> pure $ Just "Failed to get a Cancel structure"
            Just cl' -> either Just (const Nothing) <$> cancel cl'

prepareQuery :: Form
             -- ^ Form data
             -> HT.Query
             -- ^ URL query
             -> BS.ByteString
             -- ^ SQL query template
             -> (PT.Query, [Action])
             -- ^ SQL query template and parameters
prepareQuery f gq q = let sub = map substWord $ BS.words q
  in (PT.Query $ BS.unwords (map fst sub), concat $ mapMaybe snd sub)
  where
    fields = formToFields f
    bsFields = map (encodeUtf8 *** encodeUtf8) fields
    keys = map fst bsFields
    vals = map snd bsFields
    placeholders = BS.intercalate "," (map (const "?") fields)
    substWord ":fields" = (placeholders, Just (map EscapeIdentifier keys))
    substWord ":values" = (placeholders, Just (map Escape vals))
    substWord other = case BS.splitAt 2 other of
      -- POST (form) parameter
      ("f:", fieldName) -> ("?", pure . Escape <$> lookup fieldName bsFields)
      -- GET (query/link or form) parameter
      ("q:", fieldName) -> ("?", pure . Escape <$> join (lookup fieldName gq))
      _ -> (other, Nothing)

makeParams :: HT.Query -> [(String, String)]
makeParams = mapMaybe makeParam
  where makeParam (k, Just v) = Just (BS.unpack k, BS.unpack v)
        makeParam _ = Nothing

mkError :: BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString
mkError t kv = xmlElem t " xmlns=\"urn:x-pgxhtml\"" $
  BS.concat $ map (\(k,v) -> xmlElem k "" v) kv
  where
    xmlElem n a s = BS.concat ["<", n, a, ">", s, "</", n, ">"]

errorXML :: Error -> BS.ByteString
errorXML (EOther m) = mkError "error" [("message", BS.pack m)]
errorXML (EQuery (QueryError m (PT.Query q))) =
  mkError "query_error" [("message", BS.pack m), ("query", q)]
errorXML (EFormat (FormatError m (PT.Query q) p)) =
  mkError "format_error" $ [("message", BS.pack m), ("query", q)]
  ++ map ((,) "param") p
errorXML (ESQL e) = mkError "sql_error" $
  [ ("state", sqlState e)
  , ("status", BS.pack $ show $ sqlExecStatus e)
  , ("message", sqlErrorMsg e)
  , ("detail", sqlErrorDetail e)
  , ("hint", sqlErrorHint e) ]
errorXML (EResult e) = mkError "result_error" [("message", BS.pack $ errMessage e)]

serve :: EnvConfig -> IO Connection -> Application
serve conf ioc req respond = do
  form' <- urlDecodeForm <$> strictRequestBody req
  case form' of
    Left err -> respError notAcceptable406 $ EOther $
      "Failed to read form data:" ++ T.unpack err
    Right form -> case lookup "q" (queryString req) of
      Just (Just q) -> do
        let (q', params) = prepareQuery form qs q
        flip catches [ Handler $ respError badRequest400 . ESQL
                     , Handler $ respError badRequest400 . EFormat
                     , Handler $ respError badRequest400 . EQuery
                     , Handler $ respError badRequest400 . EResult
                     ]
          $ bracket ioc cancelAndClose $ \c -> do
          r <- query c q' params
          case r of
            [Only xmlDoc] -> resp ok200 (unPgXmlString xmlDoc)
            _ -> respError status500
              (EOther "Expected a single result, got 0 or more")
      _ -> respError imATeapot418 (EOther "No query is provided")
  where
    qs = queryString req
    xsltPath = xsltDirectory conf
               </> replaceExtension
               (takeFileName (BS.unpack $ rawPathInfo req)) "xsl"
    resp st xml = do
      doc <- transform xml "" xsltPath (makeParams qs)
      respond $ responseLBS st
        [(hContentType, "application/xhtml+xml")]
        (BL.fromStrict doc)
    respError st e = resp st (errorXML e)

main :: IO ()
main = do
  exsltRegisterAll
  conf <- decodeEnv
  case conf of
    Left err -> putStrLn err
    Right conf' -> defWaiMain
      $ timeout (connectionTimeout conf') $ \req respond ->
      case ("authorised" `elem` pathInfo req, extractBasicAuth
             =<< lookup hAuthorization (requestHeaders req)) of
        (True, Just (l, p)) ->
          serve conf'
          (connect $ ConnectInfo "" 0 (BS.unpack l) (BS.unpack p) "")
          req respond
        (True, Nothing) ->
          respond $ responseLBS unauthorized401
          [( hWWWAuthenticate
           , "Basic realm=\"Protected area\", charset=\"UTF-8\"")] ""
        _ -> serve conf' (connectPostgreSQL "") req respond
