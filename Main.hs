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
   writeDocumentToString, withOutputXHTML, withIndent, stringTrim,
   XNode(..), mkName)
import Text.XML.HXT.DOM.XmlNode (mkRoot)
import Text.XML.HXT.Parser.XmlParsec (xreadDoc)
import Text.XML.HXT.XSLT.Application (XPathParams, applyStylesheetWParams)
import Text.XML.HXT.XSLT.XsltArrows (xsltCompileStylesheetFromURI)
import Text.XML.HXT.XSLT.Common (XmlTree, ExName(..), Expr(..), (>>>))
import Data.Tree.Class (mkTree)
import Data.String (fromString)
import Data.Maybe (mapMaybe)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import Network.HTTP.Types.Header (hWWWAuthenticate)
import Network.Wai.Middleware.Timeout (timeout)
import Network.Wai.Cli (defWaiMain)
import Control.Monad (join)
import Control.Arrow ((***))
import Control.Exception (bracket, finally, handle)
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

errorXML :: PT.Query -> [Action] -> SqlError -> XmlTree
errorXML q p e = mkRoot []
  [mkTree (XTag (mkName "sql_error")
            [ mkAttr "template" (show q)
            , mkAttr "parameters" (show p)
            , mkAttr "state" (BS.unpack $ sqlState e)
            , mkAttr "status" (show $ sqlExecStatus e)
            , mkAttr "message" (BS.unpack $ sqlErrorMsg e)
            , mkAttr "detail" (BS.unpack $ sqlErrorDetail e)
            , mkAttr "hint" (BS.unpack $ sqlErrorHint e)
            ])
    []]
  where mkAttr k v = mkTree (XAttr $ mkName k) [mkTree (XText v) []]

serve :: EnvConfig -> IO Connection -> Application
serve conf ioc req respond = do
  form' <- urlDecodeForm <$> strictRequestBody req
  case form' of
    Left _err -> respond $ responseLBS notAcceptable406 []
      "Failed to read form data"
    Right form -> case lookup "q" (queryString req) of
      Just (Just q) -> do
        let qs = queryString req
            (q', params) = prepareQuery form qs q
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
        handle (resp badRequest400 . errorXML q' params)
          $ bracket ioc cancelAndClose $ \c -> do
          r <- query c q' params
          case r of
            [Only xmlDoc] -> resp ok200 (unPgXmlTree xmlDoc)
            _ -> respond $ responseLBS status500 []
              "Expected a single result, got 0 or more"
      _ -> respond $ responseLBS imATeapot418 [] "No query is provided"

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
