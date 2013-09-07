{-# LANGUAGE OverloadedStrings #-}

{-|

This module contains a simplified wrapper above WAI.
Notes:
    - file uploads are not working yet.

-}

module Hakit.Http (
    -- * Types
    Req,
    Resp,
    Config(..),
    -- * Server
    startServer,
    defaultConfig,
    -- * Convenience stuff
    resp,
) where

import Hakit
import qualified Data.Text.Encoding as TE
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Safe
import qualified Network.HTTP.Types as HTypes
import qualified Network.HTTP.Types.Header as HTypesHeader
-- HTTP server impl:
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as WP
import qualified Data.Conduit as Cond
import qualified Data.Map as M
import qualified Data.Word as W
import qualified Hakit.Mime as Mime
import Control.Monad.IO.Class (liftIO)

{--------------------------------------------------------------------
  Types.  
--------------------------------------------------------------------}

data Req = Req {
    method      :: T.Text,              -- | HTTP method (eg. GET, POST etc)
    path        :: [T.Text],            -- | Request path split by forward dashes.
    params      :: Document,            -- | Query params.
    reqHeaders  :: [(T.Text, T.Text)]   -- | Headers.
} deriving (Show)

-- | Response.
data Resp = Resp {
    status          :: Integer,             -- | Status code.
    contentType     :: T.Text,              -- | Response body content type.
    body            :: LBS.ByteString,      -- | Response body.
    respHeaders     :: [(T.Text, T.Text)]
} deriving (Show)

-- | Configuration needed to start up the HTTP server.
data Config = Config {
    httpPort    :: Int,                 -- | Port to listen on (default: 8080)
    memUploads  :: Bool,                -- | Store file uploads in memory.
    temp        :: T.Text               -- | Temporary folder to store uploaded files in.
} deriving (Show)

defaultConfig :: Config
defaultConfig = Config 8080 False "c:/temp"

{--------------------------------------------------------------------
  HTTP server gluing helpers.  
--------------------------------------------------------------------}

headersToDoc :: [HTypesHeader.Header] -> Document
headersToDoc h = interpretDoc $ trans h where
    trans x = map (\(key, val) -> (CI.original key, Just val)) x

docValToHeader v = case v of
    DocString s     -> TE.encodeUtf8 s
    DocInt i        -> TE.encodeUtf8 $ T.pack $ show i
    DocFloat f      -> TE.encodeUtf8 $ T.pack $ show f
    DocBool b       -> TE.encodeUtf8 $ T.pack $ show b
    otherwise       -> error $ "can't convert to header: " ++ show v

docToHeaders :: Document -> [HTypesHeader.Header]
docToHeaders d = map f $ M.toList d
    where f (k, v) = (CI.mk $ TE.encodeUtf8 k, docValToHeader v)

-- Set-Cookie: UserID=JohnDoe; Max-Age=3600; Version=1
docToCookies :: Document -> [HTypesHeader.Header]
docToCookies d = map f $ M.toList d
    where
        f (k, v) = (CI.mk $ TE.encodeUtf8 "Set-Cookie", f1 k v)
        f1 k1 v1 = BSC.concat [TE.encodeUtf8 $ T.concat [k1, "="], docValToHeader v1, "; Max-Age=360000000; Version=1"]

c2w8 :: Char -> W.Word8
c2w8 = fromIntegral . fromEnum

-- filterHeaders
filterHs :: T.Text -> [(CI.CI BS.ByteString, BS.ByteString)] -> [(CI.CI BS.ByteString, BS.ByteString)]
filterHs t hs = filter (\x -> fst x == (CI.mk $ TE.encodeUtf8 t)) hs

fromCookies :: [(CI.CI BS.ByteString, BS.ByteString)] -> Document
fromCookies hs =
    let cookieHs = filterHs "Cookie" hs
        cookieKVs = filter (\x -> not $ BS.isPrefixOf "$Version" x) $ concat $ map (BS.split (c2w8 ' ') . snd) cookieHs
        cutSemicolon x = if BS.isSuffixOf ";" x
            then BS.init x
            else x
        splitToPair x = let s = BS.split (c2w8 '=') x in
            if length s /= 2
                then error $ "malformed cookie header: " ++ show x
                else (s!!0, s!!1)
        cookieKVPairs = map (splitToPair . cutSemicolon) cookieKVs
    in interpretDoc $ map (\(a, b) -> (a, Just b)) cookieKVPairs

langsFromHeaders ::  [(CI.CI BS.ByteString, BS.ByteString)] -> [T.Text]
langsFromHeaders hs =
    let langHs = filterHs "Accept-Language" hs
    in map (\x -> TE.decodeUtf8 $ (BS.split (c2w8 ';') x)!!0) . concat $ map (BS.split (c2w8 ',') . snd) langHs 

{--------------------------------------------------------------------
  Actual HTTP server gluing (using Warp).  
--------------------------------------------------------------------}

waiTohakit :: Wai.Request -> IO Req
waiTohakit wr = do
    (paramList, files) <- Cond.runResourceT $ WP.parseRequestBody WP.tempFileBackEnd wr
    let getParams = interpretDoc $ Wai.queryString wr
        fileList = map (\(a, b) -> (a, WP.fileName b)) files
        postParams = interpretDoc . map (\(a, b) -> (a, Just b)) $ paramList ++ fileList
        verb = TE.decodeUtf8 $ Wai.requestMethod wr
        params = if verb == "GET"
            then getParams
            else postParams
        reqHs = Wai.requestHeaders wr
        -- cookies = cookiesFromHeaders reqHs
        -- langs = langsFromHeaders reqHs
    return $ Req verb (Wai.pathInfo wr) params []

statusToInt :: HTypes.Status -> Integer
statusToInt s = toInteger $ HTypes.statusCode s

intToStatus :: Integer -> HTypes.Status
intToStatus i = HTypes.Status ((fromInteger i)::Int) ""

hakitToWai :: Cond.ResourceT IO Resp -> Cond.ResourceT IO Wai.Response
hakitToWai fresp = do
    fr <- fresp
    let statusCode = intToStatus $ status fr
        headers = []
        mimeType = Mime.mimeTypeOf $ contentType fr
        mimeHeader = ("Content-Type", mimeType)
        -- storeMap = store fr
        -- unstoreMap = M.fromList . map (\x -> (x, DocString "")) $ unstore fr
        -- cookies = docToCookies $ M.union storeMap unstoreMap
    return $ Wai.responseLBS statusCode [] (body fr)

-- | Start a HTTP server. Example:
-- > startServer defaultConfig (\req -> return $ Resp OK (Body "text/html" "Hello.") emptyDoc [])
startServer :: Int -> (Req -> IO Resp) -> IO ()
startServer p reqHandler = do
    putStrLn $ "Server started listening on port " ++ show p
    Warp.run p conv
    where
        conv a = do
            a1 <- liftIO $ waiTohakit a
            hakitToWai . liftIO $ reqHandler a1

{--------------------------------------------------------------------
  Http client.  
--------------------------------------------------------------------}

-- To be implemented.

{--------------------------------------------------------------------
  Convenience stuff.  
--------------------------------------------------------------------}

-- | An empty response to use as a starting point.
-- Note you don't need something like a constructor (eg. newResp) due to Haskell's
-- purity (you can't modify values).
resp :: Resp
resp = Resp 200 "" "" []