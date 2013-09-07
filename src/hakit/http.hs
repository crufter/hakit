{-# LANGUAGE OverloadedStrings #-}

{-|

This module contains a simplified wrapper above WAI.
Notes:
    - file uploads are not working yet.

-}

module Hakit.Http (
    -- * Types
    Req,
    req,
    method,
    setMethod,
    path,
    setPath,
    params,
    setParams,
    Resp,
    resp,
    status,
    setStatus,
    body,
    setBody,
    Config(..),
    -- * Server
    startServer,
    defaultConfig,
    -- * Convenience stuff
) where

import Hakit
import qualified Data.List as L
import qualified Data.Ord as O
import qualified Data.Function as F
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
    metho       :: T.Text,              -- | HTTP method (eg. GET, POST etc)
    pat         :: [T.Text],            -- | Request path split by forward dashes.
    param       :: Document,            -- | Query params.
    reqHeaders  :: [(T.Text, T.Text)]   -- | Headers.
} deriving (Show)

-- | An empty request. See resp for more info.
req :: Req
req = Req "GET" [] nilDoc []

method :: Req -> T.Text
method = metho

setMethod :: T.Text -> Req -> Req
setMethod m r = r{metho=m}

path :: Req -> [T.Text]
path = pat

setPath :: [T.Text] -> Req -> Req
setPath ps r = r{pat=ps}

params :: Req -> Document
params = param

setParams :: Document -> Req -> Req
setParams d r = r{param=d}

-- | Response.
data Resp = Resp {
    statusCode      :: Integer,             -- | Status code.
    bod             :: LBS.ByteString,      -- | Response body.
    respHeaders     :: [(T.Text, T.Text)]
} deriving (Show)

-- | An empty response to use as a starting point.
-- Note you don't need something like a constructor (eg. newResp) due to Haskell's
-- purity (you can't modify values).
resp :: Resp
resp = Resp 200 "" []

-- | Return status code.
status :: Resp -> Integer
status res = statusCode res

-- | Set status code
setStatus :: Integer -> Resp -> Resp
setStatus i res = res{statusCode=i}

body :: Resp -> LBS.ByteString
body = bod

setBody :: LBS.ByteString -> Resp -> Resp
setBody b r = r{bod=b}

-- | A typeclass for Req and Resp types, since they all have headers
class Headery a where
    headers :: a -> [(T.Text, T.Text)]
    setHeaders :: [(T.Text, T.Text)] -> a -> a

instance Headery Req where
    headers = reqHeaders
    setHeaders hs req = req{reqHeaders=hs}

instance Headery Resp where
    headers = respHeaders
    setHeaders hs resp = resp{respHeaders=hs}

-- | Configuration needed to start up the HTTP server.
data Config = Config {
    httpPort    :: Int,                 -- | Port to listen on (default: 8080)
    memUploads  :: Bool,                -- | Store file uploads in memory.
    temp        :: T.Text               -- | Temporary folder to store uploaded files in.
} deriving (Show)

defaultConfig :: Config
defaultConfig = Config 8080 False "c:/temp"

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
        headers = respHeaders fr
        mimeType = Mime.mimeTypeOf $ contentType fr
        mimeHeader = ("Content-Type", TE.decodeUtf8 mimeType)
        waiHs = map (\(k, v) -> (CI.mk $ TE.encodeUtf8 k, TE.encodeUtf8 v)) (mimeHeader:headers)
        -- storeMap = store fr
        -- unstoreMap = M.fromList . map (\x -> (x, DocString "")) $ unstore fr
        -- cookies = docToCookies $ M.union storeMap unstoreMap
    return $ Wai.responseLBS statusCode waiHs (body fr)

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

{--------------------------------------------------------------------
  - Headers.  
--------------------------------------------------------------------}

-- | Replaces a headers' value having a key equal to the first argument
-- with the second argument.
replaceHeader :: Headery h => T.Text -> T.Text -> h -> h
replaceHeader key val h = 
    let modi hdr@(k, v) = if k == key
            then hdr
            else (k, val)
        moddedHs = map modi $ headers h
    in setHeaders moddedHs h

addHeader :: Headery h => (T.Text, T.Text) -> h -> h
addHeader hdr h = setHeaders (hdr:(headers h)) h

c2w8 :: Char -> W.Word8
c2w8 = fromIntegral . fromEnum

{--------------------------------------------------------------------
  - Cookies.  
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


fromCookies :: [(T.Text, T.Text)] -> Document
fromCookies hs =
    let cookieHs = filter (\(k, v) -> k == "Cookie") hs
        cookieKVs = filter (\x -> not $ T.isPrefixOf "$Version" x) . concat $ map (T.splitOn " " . snd) cookieHs
        cutSemicolon x = if T.isSuffixOf ";" x
            then T.init x
            else x
        splitToPair x = let s = T.splitOn "=" x in
            if length s /= 2
                then error $ "malformed cookie header: " ++ show x
                else (s!!0, s!!1)
        cookieKVPairs = map (splitToPair . cutSemicolon) cookieKVs
    in interpretDoc' cookieKVPairs

cookies :: Headery h => h -> Document
cookies res = fromCookies $ headers res

-- setCookies :: Headery h => H.Document -> h -> h
-- setCookies doc res =

{--------------------------------------------------------------------
  - Accept language.  
--------------------------------------------------------------------}

-- | Sorts languages by their Qs
-- Expected format: da, en-gb;q=0.8, en;q=0.7
languageQs :: T.Text -> [(T.Text, T.Text)] -- ("en", "0.7")
languageQs hs =
    let ls = map T.strip $ T.splitOn "," hs
        toTuples :: T.Text -> (T.Text, T.Text)
        toTuples l =
            let xs = map T.strip $ T.splitOn ";" l
            in if length xs == 2
                then (xs!!0, xs!!1)
                else (xs!!0, "1.0")
        pairs :: [(T.Text, T.Text)]
        pairs = map toTuples ls
    in L.sortBy (O.compare `F.on` snd) pairs

-- | Returns the language codes sorted by desirability.
languages :: Headery h => h -> [T.Text]
languages res =
    let lhs = filter (\(k, v) -> k == "Accept-Language") $ headers res
        lh = if length lhs == 0
            then ""
            else if length lhs == 1
                then snd $ head lhs
                else T.intercalate ", " $ map snd lhs
    in map fst $ languageQs lh

-- addLanguage :: Headery h => (T.Text, Integer) -> h -> h

{--------------------------------------------------------------------
  - Content type.  
--------------------------------------------------------------------}

contentType :: Headery h => h -> T.Text
contentType h =
    let ct = map snd $ filter (\(k, v) -> k == "Content-Type") $ headers h
    in if length ct > 0
        then head ct 
        else ""

-- | Sets the content type. Does recognize file extensions and uses
-- the appropriate mime type instead.
-- setContentType :: Headery h => T.Text -> h -> h
-- setContentType t r = r{cType=t}