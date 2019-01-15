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

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
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
import Text.XML.HXT.Core
  (IOSArrow, replaceChildren, arrL, ($<), constA, runX,
   writeDocumentToString, withOutputXHTML, withIndent, stringTrim, mkQName)
import Text.XML.HXT.DOM.XmlNode (mkRoot, mkElement, mkText)
import Text.XML.HXT.Parser.XmlParsec (xreadDoc)
import Text.XML.HXT.XSLT.Application (XPathParams, applyStylesheetWParams)
import Text.XML.HXT.XSLT.XsltArrows (xsltCompileStylesheetFromURI)
import Text.XML.HXT.XSLT.Common (XmlTree, ExName(..), Expr(..), (>>>))
import Data.String (fromString)
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

newtype PgXmlTree = PgXmlTree { unPgXmlTree :: XmlTree }

instance FromField PgXmlTree where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just mdata) = do
    tName <- typename f
    if tName /= "xml"
      then returnError ConversionFailed f $
           "Expected xml type, got " ++ BS.unpack tName
      else pure $ PgXmlTree $
           mkRoot [] $ xreadDoc (stringTrim $ BS.unpack mdata)

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

xsltApplyStylesheetWParamsFromURI :: XPathParams -> String
                                  -> IOSArrow XmlTree XmlTree
xsltApplyStylesheetWParamsFromURI par uri = replaceChildren
      $ arrL . applyStylesheetWParams par
      $< (constA uri >>> xsltCompileStylesheetFromURI)

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

makeParams :: HT.Query -> XPathParams
makeParams = M.fromList . mapMaybe makeParam
  where makeParam (k, Just v) = Just ( ExName (BS.unpack k) ""
                                     , LiteralExpr (BS.unpack v))
        makeParam _ = Nothing

mkError :: String -> [(String, String)] -> XmlTree
mkError t kv = mkRoot [] . pure $
  mkElement (mkQName "px" t nsURI) [] $
  map (\(k,v) -> mkElement (mkQName "px" k nsURI) [] [mkText v]) kv
  where
    nsURI = "urn:x-pgxhtml"

errorXML :: Error -> XmlTree
errorXML (EOther m) = mkError "error" [("message", m)]
errorXML (EQuery (QueryError m (PT.Query q))) =
  mkError "query_error" [("message", m), ("query", BS.unpack q)]
errorXML (EFormat (FormatError m (PT.Query q) p)) =
  mkError "format_error" $ [("message", m), ("query", BS.unpack q)]
  ++ map ((,) "param" . BS.unpack) p
errorXML (ESQL e) = mkError "sql_error" $
  [ ("state", BS.unpack $ sqlState e)
  , ("status", show $ sqlExecStatus e)
  , ("message", BS.unpack $ sqlErrorMsg e)
  , ("detail", BS.unpack $ sqlErrorDetail e)
  , ("hint", BS.unpack $ sqlErrorHint e) ]
errorXML (EResult e) = mkError "result_error" [("message", errMessage e)]

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
            [Only xmlDoc] -> resp ok200 (unPgXmlTree xmlDoc)
            _ -> respError status500
              (EOther "Expected a single result, got 0 or more")
      _ -> respError imATeapot418 (EOther "No query is provided")
  where
    qs = queryString req
    xsltPath = xsltDirectory conf
               </> replaceExtension
               (takeFileName (BS.unpack $ rawPathInfo req)) "xsl"
    resp st xml = do
      rc <- runX $ constA xml
            >>> xsltApplyStylesheetWParamsFromURI (makeParams qs) xsltPath
            >>> writeDocumentToString [withOutputXHTML, withIndent True]
      respond $ responseLBS st
        [(hContentType, "application/xhtml+xml")]
        (fromString $ concat $
          "<?xml version=\"1.0\"?>\n" : "<!DOCTYPE html>\n" : rc)
    respError st e = resp st (errorXML e)

main :: IO ()
main = do
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
