{-# LANGUAGE OverloadedStrings #-}

{-|

This module contains a simplified wrapper above WAI.
Notes:
    - file uploads are not working yet.

-}

module Hakit.Server (
    -- * Types
    Req(..),
    Status(..),
    Body(..),
    Resp(..),
    Config(..),
    -- * Server
    startHttp,
    defaultConfig,
    -- * Convenience functions
    quickShow,
    -- * Other
    queryToDoc
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
    verb        :: T.Text,      -- | HTTP method (eg. GET, POST etc)
    path        :: [T.Text],    -- | Request path split by forward dashes.
    params      :: Document,    -- | Query params.
    cookies     :: Document,    -- | Cookies.
    languages   :: [T.Text]     -- | Accept languages sorted by priority.
} deriving (Show)

-- | Response status.
data Status =
        OK                              -- 200
    |   Created (Maybe Location)        -- 201
    |   Moved (Maybe Location)          -- 301
    |   Redirect Location               -- 303
    |   Unauthorized                    -- 401
    |   Forbidden                       -- 403
    |   NotFound                        -- 404
    |   ServerError String              -- 500
    |   NotImplemented                  -- 501
    |   Unavailable                     -- 503
    deriving (Show)

-- | Response content body.
data Body = Body {
    ctype       :: T.Text,              -- | content type
    content     :: LBS.ByteString       -- | actual content
} deriving (Show)

-- | Response.
data Resp = Resp {
    status      :: Status,              -- | Status code.
    body        :: Body,                -- | Response body.
    store       :: Document,            -- | Cookies to store.
    unstore     :: [T.Text]             -- | Keys of cookies to delete.
                                        -- Note: we could use Nils to delete a cookie, instead a separate "unstore" field.
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
headersToDoc h = queryToDoc $ trans h where
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

-- [HTypes.QueryItem] -> Document
queryToDoc :: [(BS.ByteString, Maybe BS.ByteString)] -> Document
queryToDoc q = M.fromList $ map singlify (gr (map f q)) where
    singlify (key, docValList) = if length docValList > 1
        then (key, DocList docValList)
        else (key, Safe.atNote "docList is empty" docValList 0)
    f (key, val) = case val of
        Nothing -> (T.pack $ BSC.unpack key, Nil)
        Just bs -> (T.pack $ BSC.unpack key, interpret bs)
    iBool str = case (Safe.readMay str)::Maybe Bool of
        Just b      -> Just b
        Nothing     -> case str of
            "true"      -> Just True
            "false"     -> Just False
            otherwise   -> Nothing
    iNil str = if str == "nil" || str == "Nil" then Just Nil else Nothing
    interpret bs =
        let str = T.unpack $ TE.decodeUtf8 bs in
            case (Safe.readMay str)::Maybe Integer of
                Just i      -> d i
                Nothing     -> case (Safe.readMay str):: Maybe Double of
                    Just dbl    -> d dbl
                    Nothing     -> case iBool str of
                        Just b      -> d b
                        Nothing     -> case iNil str of
                            Just n  -> Nil
                            Nothing -> d $ T.pack str

c2w8 :: Char -> W.Word8
c2w8 = fromIntegral . fromEnum

-- filterHeaders
filterHs :: T.Text -> [(CI.CI BS.ByteString, BS.ByteString)] -> [(CI.CI BS.ByteString, BS.ByteString)]
filterHs t hs = filter (\x -> fst x == (CI.mk $ TE.encodeUtf8 t)) hs

cookiesFromHeaders :: [(CI.CI BS.ByteString, BS.ByteString)] -> Document
cookiesFromHeaders hs =
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
    in queryToDoc $ map (\(a, b) -> (a, Just b)) cookieKVPairs

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
    let getParams = queryToDoc $ Wai.queryString wr
        fileList = map (\(a, b) -> (a, WP.fileName b)) files
        postParams = queryToDoc . map (\(a, b) -> (a, Just b)) $ paramList ++ fileList
        verb = TE.decodeUtf8 $ Wai.requestMethod wr
        params = if verb == "GET"
            then getParams
            else postParams
        reqHs = Wai.requestHeaders wr
        cookies = cookiesFromHeaders reqHs
        langs = langsFromHeaders reqHs
    return $ Req verb (Wai.pathInfo wr) params cookies langs

hakitToWai :: Cond.ResourceT IO Resp -> Cond.ResourceT IO Wai.Response
hakitToWai fresp = do
    fr <- fresp
    let statusCode = case status fr of
            OK                    -> HTypes.status200
            Created mloc          -> HTypes.status201
            Moved mloc            -> HTypes.status301
            Redirect loc          -> HTypes.status303
            Unauthorized          -> HTypes.status401
            Forbidden             -> HTypes.status403
            NotFound              -> HTypes.status404
            ServerError s         -> HTypes.status500
            NotImplemented        -> HTypes.status501
            Unavailable           -> HTypes.status503
        mimeType = Mime.mimeTypeOf . ctype $ body fr
        contentType = ("Content-Type", mimeType)
        cookies = docToCookies $ store fr
    return $ Wai.responseLBS statusCode (contentType:cookies) (content $ body fr)

-- | Start a HTTP server. Example:
-- > startServer defaultConfig (\req -> return $ Resp OK (Body "text/html" "Hello.") emptyDoc [])
startHttp :: Int -> (Req -> IO Resp) -> IO ()
startHttp p reqHandler = do
    putStrLn $ "Server started listening on port " ++ show p
    Warp.run p conv
    where
        conv a = do
            a1 <- liftIO $ waiTohakit a
            hakitToWai . liftIO $ reqHandler a1

{--------------------------------------------------------------------
  Convenience functions.  
--------------------------------------------------------------------}

quickShow :: Show a => T.Text -> a -> Resp
quickShow t a = Resp OK (Body t $ LBS.fromChunks [BSC.pack $ show a]) nilDoc []