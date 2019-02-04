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
import Network.Wai
import Network.HTTP.Types as HT
import Web.FormUrlEncoded (Form(..), urlDecodeForm)
import Database.PostgreSQL.LibPQ
  (getCancel, cancel, connectdb, finish, execParams, getvalue, invalidOid,
   escapeIdentifier, resultStatus, errorMessage, Connection, Format(..),
   ExecStatus(..))
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Header (hWWWAuthenticate)
import System.Timeout (timeout)
import Network.Wai.Handler.FastCGI (run)
import Control.Monad (join)
import Control.Arrow ((***))
import Control.Exception (bracket, finally)
import System.FilePath (replaceExtension, takeFileName, (</>))
import System.Environment (lookupEnv)
import Data.List (nubBy)
import Data.ByteString.Base64 (decodeLenient)
import Foreign
import Foreign.C


-- * libxml bindings and functions

-- http://xmlsoft.org/html/index.html
-- http://xmlsoft.org/XSLT/html/index.html

data XmlDoc
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
  bracket
  (notNull $ xmlReadMemory docCStr (fromIntegral docCStrLen) baseCStr nullPtr 0)
  xmlFreeDoc $ \doc ->
  bracket (mapM newCString (concatMap (\(x, y) -> [x, y]) $
                            nubBy (\x y -> fst x == fst y) stringParams))
  (mapM free) $ \params ->
  withArray0 nullPtr params $ \paramsArr ->
  withArray0 nullPtr [] $ \emptyArr ->
  bracket (notNull $ xsltParseStylesheetFile pathCStr) xsltFreeStylesheet $
  \stylesheet ->
  bracket (notNull $ xsltNewTransformContext stylesheet doc)
  xsltFreeTransformContext $
  \tc -> xsltQuoteUserParams tc paramsArr >>
  bracket
  (notNull $ xsltApplyStylesheetUser stylesheet doc emptyArr nullPtr nullPtr tc)
  xmlFreeDoc
  (\res ->
      xsltSaveResultToString bufPtr lenPtr res stylesheet >>
      bracket (peek bufPtr) free BS.packCString)
  where
    notNull :: IO (Ptr a) -> IO (Ptr a)
    notNull a = a >>= \p -> if p == nullPtr
                            then error "Unexpected NULL pointer"
                            else pure p
  -- TODO: improve error handling


-- * PostgreSQL-related functions

connString :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
connString [] = BS.empty
connString ((k,v):xs) =
  BS.concat [k, "='", BS.pack (val $ BS.unpack v), "' ", connString xs]
  where val [] = []
        val ('\\':cs) = '\\' : '\\' : val cs
        val ('\'':cs) = '\\' : '\'' : val cs
        val (c:cs) = c : val cs

-- https://www.postgresql.org/docs/11/libpq-exec.html
-- https://hackage.haskell.org/package/postgresql-libpq-0.9.4.2/docs/Database-PostgreSQL-LibPQ.html
prepareQuery :: Connection
             -> [(BS.ByteString, BS.ByteString)]
             -- ^ Form data
             -> [(BS.ByteString, Maybe BS.ByteString)]
             -- ^ URL query
             -> BS.ByteString
             -- ^ SQL query template
             -> IO (BS.ByteString, [BS.ByteString])
             -- ^ SQL query template and parameters
prepareQuery c f gq q = substWords [] [] $ BS.words q
  where
    placeholder :: Int -> BS.ByteString
    placeholder n = BS.cons '$' $ BS.pack $ show $ n + 1
    placeholders :: Int -> BS.ByteString
    placeholders cnt = BS.intercalate " , "
      [placeholder (cnt + n) | n <- [0 .. length f - 1]]
    substWords :: [BS.ByteString]
               -- ^ query bits
               -> [BS.ByteString]
               -- ^ parameters
               -> [BS.ByteString]
               -- ^ remaining words
               -> IO (BS.ByteString, [BS.ByteString])
               -- ^ query and parameters
    substWords qs ps [] = pure (BS.unwords qs, ps)
    substWords qs ps (":fields":rest) = do
      identifiers <- BS.intercalate ", " . catMaybes
                     <$> mapM (escapeIdentifier c . fst) f
      substWords (qs ++ [identifiers]) ps rest
    substWords qs ps (":values":rest) =
      substWords (qs ++ [placeholders $ length ps]) (ps ++ map snd f) rest
    substWords qs ps (other:rest) = case BS.splitAt 2 other of
      -- POST (form) parameter
      ("f:", fieldName) -> case lookup fieldName f of
        Nothing -> substWords (qs ++ [other]) ps rest
        Just v -> substWords (qs ++ [placeholder $ length ps]) (ps ++ [v]) rest
      -- GET (query/link or form) parameter
      ("q:", fieldName) -> case join (lookup fieldName gq) of
        Nothing -> substWords (qs ++ [other]) ps rest
        Just v -> substWords (qs ++ [placeholder $ length ps]) (ps ++ [v]) rest
      _ -> substWords (qs ++ [other]) ps rest


-- * Web interface

formToFields :: Form -> [(T.Text, T.Text)]
formToFields = mapMaybe toField . HM.toList . unForm
  where toField (f, [v]) = Just (f, v)
        toField _ = Nothing

cancelAndClose :: Connection -> IO (Maybe BS.ByteString)
cancelAndClose c = cancelConn `finally` finish c
  where cancelConn = do
          cl <- getCancel c
          case cl of
            Nothing -> pure $ Just "Failed to get a Cancel structure"
            Just cl' -> either Just (const Nothing) <$> cancel cl'

makeParams :: HT.Query -> [(String, String)]
makeParams = mapMaybe makeParam
  where makeParam (k, Just v) = Just (BS.unpack k, BS.unpack v)
        makeParam _ = Nothing

errorXML :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
errorXML kv = xmlElem "error" " xmlns=\"urn:x-pgxhtml\"" $
  BS.concat $ map (\(k,v) -> xmlElem k "" v) kv
  where
    xmlElem n a s = BS.concat ["<", n, a, ">", s, "</", n, ">"]

serve :: FilePath -> IO Connection -> Application
serve xsltDirectory ioc req respond = do
  form' <- urlDecodeForm <$> strictRequestBody req
  case form' of
    Left err -> respError notAcceptable406
      [("message", BS.pack $ "Failed to read form data:" ++ T.unpack err)]
    Right form -> case join $ lookup "q" $ queryString req of
      Just q -> bracket ioc cancelAndClose $ \c -> do
        (q', params) <- prepareQuery c
          (map (encodeUtf8 *** encodeUtf8) $ formToFields form) qs q
        r <- execParams c q'
          (map (\p -> Just (invalidOid, p, Text)) params) Text
        case r of
          Just r' -> do
            rs <- resultStatus r'
            case rs of
              TuplesOk -> do
                -- TODO: add more checks and error messages
                val <- getvalue r' 0 0
                case val of
                  Nothing -> respError status500
                    [("message", "Failed to read query result")]
                  Just val' -> resp ok200 val'
              CommandOk ->
                respError status500
                  [("message", "The command didn't return XML")]
              _ -> do
                errMsg <- maybe [] (\m -> [("message", m)]) <$> errorMessage c
                respError status500 $
                  ("exec_status", BS.pack (show rs)) : errMsg
          Nothing -> respError status500
            [("message", "Failed to execute the query")]
      _ -> respError imATeapot418 [("message", "No query is provided")]
  where
    qs = queryString req
    xsltPath = xsltDirectory
               </> replaceExtension
               (takeFileName (BS.unpack $ rawPathInfo req)) "xsl"
    resp st xml = do
      doc <- transform xml "" xsltPath (makeParams qs)
      respond $ responseLBS st
        [(hContentType, "application/xhtml+xml")]
        (BL.fromStrict doc)
    respError st e = resp st (errorXML e)

-- https://tools.ietf.org/html/rfc7235
-- https://tools.ietf.org/html/rfc7617
baCredentials :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
baCredentials cred = do
  (login, password) <-
    BS.break (== ':') . decodeLenient <$> BS.stripPrefix "Basic " cred
  if BS.length password > 0
    then Just (login, BS.tail password)
    else Nothing

main :: IO ()
main = do
  exsltRegisterAll
  xsltDir <- fromMaybe "." <$> lookupEnv "XSLT_DIR"
  to <- maybe 10 read <$> lookupEnv "TIMEOUT"
  run $ \req respond ->
    maybe (respond $ responseLBS status504 [] "") pure =<<
    timeout (to * 10 ^ (6 :: Int))
    (case ("authorised" `elem` pathInfo req, baCredentials
            =<< lookup hAuthorization (requestHeaders req)) of
        (True, Just (l, p)) ->
          serve xsltDir
          (connectdb (connString [("user", l), ("password", p)]))
          req respond
        (True, Nothing) ->
          respond $ responseLBS unauthorized401
          [( hWWWAuthenticate
           , "Basic realm=\"Protected area\", charset=\"UTF-8\"")] ""
        _ -> serve xsltDir (connectdb "") req respond)
