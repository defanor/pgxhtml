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

#include <libxslt/xsltInternals.h>

import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.LibPQ
import Data.Maybe (mapMaybe, fromMaybe, catMaybes, fromJust)
import System.Timeout (timeout)
import Control.Exception (bracket, finally)
import System.FilePath (takeFileName, (</>), (<.>))
import System.Environment (lookupEnv)
import Data.List (nubBy)
import Data.ByteString.Base64 (decodeLenient)
import Data.Char (ord, chr)
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
          -> [(BS.ByteString, BS.ByteString)]
          -- ^ string params
          -> IO (BS.ByteString, Maybe BS.ByteString)
transform docBS baseStr pathStr stringParams =
  BS.useAsCStringLen docBS $ \(docCStr, docCStrLen) ->
  withCString baseStr $ \baseCStr ->
  withCString pathStr $ \pathCStr ->
  alloca $ \bufPtr ->
  alloca $ \lenPtr ->
  bracket
  (notNull $ xmlReadMemory docCStr (fromIntegral docCStrLen) baseCStr nullPtr 0)
  xmlFreeDoc $ \doc ->
  useAsCStrings (concatMap (\(x, y) -> [x, y]) $
                  nubBy (\x y -> fst x == fst y) stringParams) [] $ \params ->
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
  (\res -> do
      xsltSaveResultToString bufPtr lenPtr res stylesheet
      bracket (peek bufPtr) free $ \resultCStr -> do
        resultBS <- BS.packCString resultCStr
        mt <- mediaType stylesheet
        pure (resultBS, mt))
  where
    useAsCStrings :: [BS.ByteString] -> [CString] -> ([CString] -> IO a) -> IO a
    useAsCStrings [] a f = f a
    useAsCStrings (x:xs) a f = BS.useAsCString x $ \x' ->
      useAsCStrings xs (a ++ [x']) f
    notNull :: IO (Ptr a) -> IO (Ptr a)
    notNull a = a >>= \p -> if p == nullPtr
                            then error "Unexpected NULL pointer"
                            else pure p
    mediaType :: Ptr XsltStylesheet -> IO (Maybe BS.ByteString)
    mediaType pXslt = do
      mt <- (#peek xsltStylesheet, mediaType) pXslt
      if mt == nullPtr
        then pure Nothing
        else Just <$> BS.packCString mt
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
             -> [(BS.ByteString, BS.ByteString)]
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
      ("q:", fieldName) -> case lookup fieldName gq of
        Nothing -> substWords (qs ++ [other]) ps rest
        Just v -> substWords (qs ++ [placeholder $ length ps]) (ps ++ [v]) rest
      _ -> substWords (qs ++ [other]) ps rest

cancelAndClose :: Connection -> IO (Maybe BS.ByteString)
cancelAndClose c = cancelConn `finally` finish c
  where cancelConn = do
          cl <- getCancel c
          case cl of
            Nothing -> pure $ Just "Failed to get a Cancel structure"
            Just cl' -> either Just (const Nothing) <$> cancel cl'


-- * CGI and HTTP utilities

-- https://tools.ietf.org/html/rfc3875
-- https://tools.ietf.org/html/rfc2396

-- https://www.w3.org/TR/html5/sec-forms.html
-- https://url.spec.whatwg.org/
parseFormUrlencoded :: BS.ByteString -> [(BS.ByteString, BS.ByteString)]
parseFormUrlencoded s = mapMaybe seq2nv $ BS.split '&' s
  where
    seq2nv sq
      | BS.null sq = Nothing
      | otherwise = let (n, v') = BS.break (== '=') sq
                        v = if BS.null v' then v' else BS.tail v'
                    in Just (unescape n, unescape v)
    unescape :: BS.ByteString -> BS.ByteString
    unescape bs = case BS.uncons bs of
      Nothing -> bs
      Just ('+', rest) -> BS.cons ' ' (unescape rest)
      Just ('%', rest) -> case BS.uncons rest of
        Nothing -> BS.pack ['%']
        Just (c1, rest') -> case BS.uncons rest' of
          Nothing -> BS.pack ['%', c1]
          Just (c2, rest'') -> case (parseChar c1, parseChar c2) of
            (Just c1', Just c2') ->
              BS.cons (chr $ c1' * 0x10 + c2') (unescape rest'')
            _ -> BS.cons '%' (unescape rest)
      Just (c, rest) -> BS.cons c (unescape rest)
    parseChar :: Char -> Maybe Int
    parseChar c
      | c >= 'A' && c <= 'F' = Just $ ord c - ord 'A' + 10
      | c >= '0' && c <= '9' = Just $ ord c - ord '0'
      | otherwise = Nothing

-- https://tools.ietf.org/html/rfc7235
-- https://tools.ietf.org/html/rfc7617
baCredentials :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
baCredentials cred = do
  (login, password) <-
    BS.break (== ':') . decodeLenient <$> BS.stripPrefix "Basic " cred
  if BS.length password > 0
    then Just (login, BS.tail password)
    else Nothing

respond' :: Int -> [String] -> BS.ByteString -> IO ()
respond' code headers content = do
  mapM_ putStrLn headers
  putStrLn $ concat ["Status:", show code, " ", reason, "\n"]
  BS.putStr content
  where
    reason = case code of
      200 -> "OK"
      401 -> "Unauthorized"
      418 -> "I'm a teapot"
      504 -> "Gateway Timeout"
      _ -> ""

respond :: Int -> BS.ByteString -> IO ()
respond c = respond' c []

requireAuth :: IO ()
requireAuth = respond' 401
  ["WWW-Authenticate:Basic realm=\"Protected area\", charset=\"UTF-8\""]
  ""

-- * Main routines

errorXML :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
errorXML kv = xmlElem "error" " xmlns=\"urn:x-pgxhtml\"" $
  BS.concat $ map (\(k,v) -> xmlElem k "" v) kv
  where
    xmlElem n a s = BS.concat ["<", n, a, ">", s, "</", n, ">"]

serve :: FilePath -> IO Connection -> [(BS.ByteString, BS.ByteString)] -> IO ()
serve xsltDirectory ioc ps = case lookup "q" ps of
  Just q -> bracket ioc cancelAndClose $ \c -> do
    let redb n e = respError n =<<
          (maybe e (\em' -> ("db_error", em') : e) <$> errorMessage c)
    cStatus <- status c
    case cStatus of
      ConnectionOk -> do
        formData <- parseFormUrlencoded <$> BS.getContents
        (q', params) <- prepareQuery c formData ps q
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
                  Nothing ->
                    redb 500 [("message", "Failed to read query result")]
                  Just val' -> resp 200 val'
              CommandOk ->
                redb 500 [("message", "The command didn't return XML")]
              _ -> do
                errMsg <- maybe [] (\m -> [("message", m)]) <$> errorMessage c
                redb 500 $ ("exec_status", BS.pack (show rs)) : errMsg
          Nothing -> redb 500 [("message", "Failed to execute the query")]
      _ -> do
        pNeeded <- connectionNeedsPassword c
        pUsed <- connectionUsedPassword c
        if pNeeded || pUsed
          then requireAuth
          else redb 500 [("message", "Database connection failed")]
  _ -> respError 418 [("message", "No query is provided")]
  where
    xsltPath = xsltDirectory </>
               takeFileName (BS.unpack $ fromMaybe "default" $ lookup "t" ps)
               <.> "xsl"
    resp st xml = do
      (doc, mt) <- transform xml "" xsltPath ps
      BS.putStrLn $ BS.append "Content-Type:" $
        fromMaybe "application/xhtml+xml" mt
      respond st doc
    respError st e = resp st (errorXML e)

main :: IO ()
main = do
  exsltRegisterAll
  -- It's okay to fail when not invoked properly.
  ps <- parseFormUrlencoded . BS.pack . fromJust <$> lookupEnv "QUERY_STRING"
  ha <- lookupEnv "HTTP_AUTHORIZATION"
  xsltDir <- fromMaybe "." <$> lookupEnv "XSLT_DIR"
  to <- maybe 10 read <$> lookupEnv "TIMEOUT"
  maybe (respond 504 "") pure =<< timeout (to * 10 ^ (6 :: Int))
    (case (lookup "auth" ps, baCredentials =<< BS.pack <$> ha) of
       (_, Just (l, p)) ->
         serve xsltDir (connectdb (connString [("user", l), ("password", p)]))
         ps
       (Just "on", Nothing) -> requireAuth
       _ -> serve xsltDir (connectdb "") ps)
