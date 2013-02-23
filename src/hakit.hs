{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Hakit (
    -- * Document related types.
    DMap, DList, DTyped(..), DocVal(..), Document, Collection,
    DocValComp(..), DocComp(..),
    
    -- * Convenience.
    isInt, toInt, isFloat, toFloat, isString, toString, isBool, toBool, isMap, toMap, isList, toList, isNil, toNil, len,
    getString, getInt, getFloat, getBool, getMap, getList, getNil,
    d, dt, (.-), dbRefToStr, strToDbRef,
    emptyDoc,
    
    -- * Database related.
    Db(..), Specials(..), specials, docToSpecials, toDocStyleSort,
    
    -- * Operations on documents.
    get, exists, set, unset, filt, flatten, ma,
    
    -- * Server related.
    Req(..), Resp(..), Status(..), Body(..), Location(..),
    parseLocation
) where

import qualified Data.List as List
import qualified Data.List.Split as Spl
import qualified Safe
import qualified Data.ByteString.Lazy as LBS -- hehe
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as HTypes
import qualified Network.HTTP.Types.Header as HTypesHeader
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Function as F
import qualified Data.Ord as O
import qualified Data.Map as M
import qualified Data.CaseInsensitive as CI
-- HTTP server impl:
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Data.Conduit as Cond
-- import qualified Hframe.ExtMime as EM
import Control.Monad.IO.Class (liftIO)

{--------------------------------------------------------------------
  Document.  
--------------------------------------------------------------------}

type DMap = M.Map T.Text DocVal
type DList = [DocVal]
-- Allows us to store some metainformation along with the DocVal.
data DTyped = DTyped {
    typ::T.Text,
    val::DocVal
} deriving (Ord, Eq, Show)
data DocVal =   DocInt          Integer
                | DocFloat      Double
                | DocString     T.Text
             -- | DocBS         ByteString
                | DocBool       Bool
                | DocMap        DMap
                | DocList       DList
                | DocTyped      DTyped     
                | Nil
                deriving (Ord, Eq, Show)

isInt a =       case a of DocInt b -> True; otherwise -> False
toInt a =       case a of DocInt b -> a; otherwise -> error $ show a ++ " is not an Integer."
isFloat a =     case a of DocFloat b -> True; otherwise -> False
toFloat a =     case a of DocFloat b -> b; otherwise -> error $ show a ++ " is not a Float."
isString a =    case a of DocString b -> True; otherwise -> False
toString a =    case a of DocString b -> b; otherwise -> error $ show a ++ " is not a String."
isBool a =      case a of DocBool b -> True; otherwise -> False
toBool a =      case a of DocBool b -> b; otherwise -> error $ show a ++ " is not a Bool."
isMap a =       case a of DocMap b -> True; otherwise -> False
toMap a =       case a of DocMap b -> b; otherwise -> error $ show a ++ " is not a Map."
isList a =      case a of DocList b -> True; otherwise -> False
toList a =      case a of DocList b -> b; otherwise -> error $ show a ++ " is not a List."
isNil a =       case a of Nil -> True; otherwise -> False
toNil a =       case a of Nil -> Nil; otherwise -> error $ show a ++ " is not a Nil."
len a = case a of
    DocString s -> T.length s
    DocList l   -> length l
    DocMap m    -> M.size m
    otherwise   -> error "len applied on incompatible DocVal type."

-- "DocValCompatible" class for types which know how to convert themself to a DocVal.
class (Show a, Eq a) => DocValComp a where
	toDocVal :: a -> DocVal

instance DocValComp Integer where
	toDocVal = DocInt

instance DocValComp Double where
	toDocVal = DocFloat

instance DocValComp String where
    toDocVal = DocString . T.pack

instance DocValComp Bool where
    toDocVal = DocBool

instance DocValComp DMap where
    toDocVal = DocMap

instance DocValComp DList where
    toDocVal = DocList

instance DocValComp DTyped where
    toDocVal = DocTyped

instance DocValComp DocVal where
    toDocVal = id

instance DocValComp T.Text where
    toDocVal = DocString

instance DocValComp [(T.Text, DocVal)] where
    toDocVal = DocMap . M.fromList

-- instance DocValComp a => DocValComp [a] where
--     toDocVal l = DocList $ map toDocVal l

-- | Converts a compatible type into a docval.
d :: DocValComp a => a -> DocVal
d a = toDocVal a

-- | Shorthand to create a DocTyped value.
dt :: DocValComp a => T.Text -> a -> DocVal
dt typ val = DocTyped $ DTyped typ $ toDocVal val

-- | Just a short synonym for M.fromList.
dm a = M.fromList a

infix 0 .-
-- | Helps to easily create a document (compatible type), like ["name" .- "Joey"]
(.-) :: DocValComp b => T.Text -> b -> (T.Text, DocVal)
(.-) a b = (a, toDocVal b)

{--------------------------------------------------------------------
  Document operations.  
--------------------------------------------------------------------}

-- One level of access path.
data AccElem = AccStr T.Text | AccInt Integer deriving (Show)

get' :: AccElem -> DocVal -> (DocVal, Bool)
get' a b = let notFound = (Nil, False) in case a of
    AccStr s    -> case b of
        DocMap m    -> case M.lookup s m of
            Just dv -> (dv, True)
            Nothing -> notFound
        otherwise   -> notFound
    AccInt i    -> case b of
        DocList l   -> if (length l) > (fromIntegral i) then (Safe.atNote "at get'" l (fromIntegral i), True) else notFound
        otherwise   -> notFound

parseAccElems :: T.Text -> [AccElem]
parseAccElems a = map parseOne parts where
    unp = T.unpack a 
    parts = Spl.split ((Spl.dropBlanks . Spl.dropDelims . Spl.oneOf) ".[]") unp
    parseOne a = let x = Safe.readMay a in
        case x of
            Just x      -> AccInt (toInteger x)
            Nothing     -> AccStr $ T.pack a

getRec :: T.Text -> Document -> (DocVal, Bool)
getRec accPathStr doc = getRecurs accElems (DocMap doc) where
    accElems = parseAccElems accPathStr
    getRecurs :: [AccElem] -> DocVal -> (DocVal, Bool)
    getRecurs elems docval
        | length elems == 1     = get' (Safe.atNote "getRec 1" elems 0) docval
        | length elems > 1      = getRecurs (tail elems) $ fst $ get' (Safe.atNote "getRec 2" elems 0) docval

-- | Returns nil if the value under the access path is Nil or that access path is nonexistent.
-- To differentiate between Nils and nonexistent access pathes see hasKey.
-- Example usage: get "author.books[1].title"
get :: T.Text -> Document -> DocVal
get accPathStr doc = fst $ getRec accPathStr doc

-- Helper functions for fast retrieval when you are sure the element is present.
getString   a b = case get a b of DocString s   -> s;   otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a Text."
getInt      a b = case get a b of DocInt i      -> i;   otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not an Int."
getFloat    a b = case get a b of DocFloat f    -> f;   otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a Float."
getBool     a b = case get a b of DocBool b     -> b;   otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a Bool."
getList     a b = case get a b of DocList l     -> l;   otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a List."
getMap      a b = case get a b of DocMap m      -> m;   otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a Map."
getNil      a b = case get a b of Nil           -> Nil; otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a Nil."
getTyped    a b = case get a b of DocTyped t    -> t;   otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not Typed."

exists :: T.Text -> Document -> Bool
exists accPathStr doc = snd $ getRec accPathStr doc

set :: DocValComp d => T.Text -> Document -> d -> Document
set key doc val = M.update (\_ -> Just $ toDocVal val) key doc

unset :: T.Text -> Document -> Document
unset key doc = M.delete key doc

-- | filter recursively. Both the compound data structures (map, list, doctyped) and all of its elements will be feeded to the predicate.
-- Note: in maps, only the value part of the key-value pairs are feeded to the predicate.
filt :: (DocVal -> Bool) -> DocVal -> [DocVal]
filt f d = filtRec f d where
    lif     f d = if f d == True then [d] else []
    filtRec f d = case d of
        DocMap m    -> lif f d ++ concat (map (\(a, b) -> filtRec f b) (M.toList m)) -- There may be a more efficient solution than M.toList
        DocList l   -> lif f d ++ concat (map (\x -> filtRec f x) l)
        DocTyped t  -> lif f d ++ filtRec f (val t)
        otherwise   -> lif f d

-- | Returns all elements in a DocVal (which can be a map, list, or a single element)
flatten :: DocVal -> [DocVal]
flatten d = filt (\x -> True) d

-- | map recursively.
-- Note: see note of filt.
ma :: (DocVal -> DocVal) -> DocVal -> DocVal
ma f d = maRec f d where
    maRec :: (DocVal -> DocVal) -> DocVal -> DocVal
    maRec f d = case d of
        DocMap m    -> f $ DocMap $     M.map (\a -> maRec f a) m
        DocList l   -> f $ DocList $    map (maRec f) l
        DocTyped t  -> f $ DocTyped $   DTyped (typ t) $ maRec f (val t)
        otherwise   -> f d

{--------------------------------------------------------------------
  Oid/dbRef helpers.  
--------------------------------------------------------------------}

oid :: T.Text -> DocVal
oid x = DocTyped $ DTyped "id" (DocString x)

refSep      = " "
idKey       = "id"
refType     = "dbRef"
idType      = "id"
regexpType  = "regexp"
idLen       = 24

dbRef :: DocComp dc => dc -> DocVal
dbRef doc = DocTyped $ DTyped "dbRef" (d (toDoc doc))

unpackDbRef :: DTyped -> Document
unpackDbRef x = case x of
    DTyped{typ="dbRef", val=DocMap m}   -> m
    otherwise                           -> error "Can't convert DTyped to a dbRef."

idOf :: DTyped -> T.Text
idOf x = getString "id" $ unpackDbRef x

strToId :: T.Text -> DocVal
strToId str = d $ DTyped "id" (d str)

dbRefMustBe = "A dbRef must be of type DTyped"

idOf' :: DocVal -> T.Text
idOf' x = case x of
    DocTyped d  -> idOf d
    otherwise   -> error dbRefMustBe

collOf :: DTyped -> Collection
collOf x = getString "coll" $ unpackDbRef x

collOf' :: DocVal -> T.Text
collOf' x = case x of
    DocTyped d  -> collOf d
    otherwise   -> error dbRefMustBe

{--------------------------------------------------------------------
  These functions are useful when you implement the Db class.  
--------------------------------------------------------------------}

-- | find, findOne and iterate Selectors may contain the following fields which have special meaning:
specialKeys :: M.Map T.Text ()
specialKeys = let sp = ["sort", "limit", "skip", "page", "location"] in
    M.fromList $ map (\x -> (x, ())) sp

-- | Extracts the special fields out of a Document. First is the specials.
specials :: Document -> (Document, Document)
specials doc = M.partitionWithKey (\k _ -> M.member k specialKeys) doc

data Specials = Specials {
    sort                :: [T.Text],    -- Example: ["x", "-y"]
    limit, skip, page   :: Integer,
    loc                 :: DMap         -- Example: ["server" .- "main", "db" .- "testDb", "coll" .- "testCollection"]
}

-- ["x", "-y"] -> ["x" .- 1, "y" .- (-1)]
toDocStyleSort :: [T.Text] -> Document
toDocStyleSort a =
    let f x = if T.isInfixOf "-" x
        then (T.tail x) .- (-1::Integer)
        else x .- (1::Integer)
    in M.fromList $ map f a

dvToStr docval = case docval of
    DocString s -> s
    otherwise -> ""     -- This is kind of a strange default.
dvToStrList docval = case docval of
    DocList l -> map dvToStr l
    otherwise -> []
dvToI docval = case docval of
    DocInt i -> i
    otherwise -> 0
dvToM docval = case docval of
    DocMap m -> m
    otherwise -> M.empty
docToSpecials doc =
    let spec = fst (specials doc)   -- Just to decrease algorightmic complexity.
        sort = dvToStrList (get "sort" spec)
        lim = dvToI (get "limit" spec)
        skip = dvToI (get "skip" spec)
        page = dvToI (get "page" spec)
        loc = dvToM (get "location" spec) in
    Specials sort lim skip page loc

type Document = DMap
type Collection = T.Text

emptyDoc :: M.Map T.Text DocVal
emptyDoc = M.empty

size a = M.size a

class DocComp a where
    toDoc :: a -> Document

instance DocComp [(T.Text, DocVal)] where
    toDoc = M.fromList

instance DocComp Document where
    toDoc = id

nil :: [(T.Text, DocVal)]
nil = []

{--------------------------------------------------------------------
  The database class.  
--------------------------------------------------------------------}

-- | Database connection class.
-- A full location is ["server" .- "serveraddr.com:27017", db .- "dbName", coll .- "users"]
-- But a location can be partial too (eg a server only, or a server and a db).
-- You can specify a location in a Selector, by providing the field ["location" .- ...], or to avoid repetition,
-- you can set the default location for the given db connection with the @setLocation@ method.
class Db db where
    -- | Connects to a db server. Can be connected to multiple servers. Querying a server which the db handle has no connection with will cause
    -- a temporal connection to take place.
    -- In the input document d, if you specify "user" and "password" fields, it will try to authenticate the connection.
    -- Exception will be thrown if the authentication is tried but fails.
    -- Authentication happens against a database, so a database must be specified.
    connect     :: DocComp d => db -> d -> IO db
    -- TODO: auth function.
    -- | List of servers addresses the given db handle is connected to.
    servers     :: db -> [T.Text]
    -- | Get location.
    location    :: db -> Document
    -- | Sets default location.
    setLocation :: DocComp d => db -> d -> db
    -- | Drop a database.
    dropDb      :: DocComp d => db -> d -> IO ()
    -- Drop the current database.
    dropCurrent :: db -> IO ()
    -- Insert a document. Returned string is the Id of the freshly inserted document. If a document already has an "id" field, that will be used.
    insert      :: DocComp d => db -> d -> IO T.Text
    -- Insert multiple documents at once, return their Ids.
    insertAll   :: DocComp d => db -> [d] -> IO [T.Text]
    -- Find all documents in selection.
    find        :: DocComp s => db -> s -> IO [Document]
    -- Find the first document in selection.
    findOne     :: DocComp s => db -> s -> IO (Maybe Document)
    -- Update (modify) the document in selection.
    update      :: (DocComp s, DocComp mod) => db -> s -> mod -> IO ()
    -- Count the documents in selection.
    count       :: DocComp s => db -> s -> IO Integer
    -- Delete all documents in selection.
    delete      :: DocComp s => db -> s -> IO ()
    -- Delete first document in selection.
    deleteOne   :: DocComp s => db -> s -> IO ()
    -- Replace first document in selection with given document.
    replace     :: DocComp s => db -> s -> Document -> IO ()
    -- Iterate over documents in selection. Useful for batch processing.
    iterate     :: DocComp s => db -> s -> (Document -> IO ()) -> IO ()
    -- Maybe add something like iterateBatch :: DocComp s => db -> s ([Document] -> IO ()) -> IO ()
    -- Resolve all dbRefs in given set of documents.
    resolve     :: db -> [Document] -> IO [Document]
    resolve db docs = resolveDbRefs db docs

isXTyped t x = case x of
    DocTyped d -> typ d == t;
    otherwise -> False
isDbRef x = isXTyped "dbRef" x
isId x = isXTyped "id" x

dbRefToStr doc = T.intercalate ";" $ map (\(a, b) -> T.concat [toString b, "|", a]) $ List.sortBy (O.comparing fst) (M.toList doc)

strToDbRef str = DocTyped $ DTyped "dbRef" $ DocMap $ M.fromList $
    map (\x -> let xs = T.splitOn "|" x in (Safe.atNote "strToDbRef 1" xs 1, d $ Safe.atNote "strToDbRef 0" xs 0)) (T.splitOn ";" str)

idToStr x = case x of
    DocTyped DTyped{typ="id",val=DocString s} -> s;
    otherwise -> error "DocVal is not an id."

-- find documents, then [Document] -> [("id-collname", Document)] so we can build a map out of it easily.
toCollIdPairs db (grouperStr, location, idList) = do
    docs <- find (setLocation db location) $ dm ["_id" .- ["$in" .- idList]]
    return $ map (\dc -> (T.concat [idToStr (get "id" dc), "-", grouperStr], dc)) docs

docValToDoc x = case x of
    DocMap x -> x;
    otherwise -> error "DocVal is not a DocMap" 

resolveDbRefs :: Db a => a -> [Document] -> IO [Document] 
resolveDbRefs db docs = do
    let refs        = List.concat $ map (\x -> filt isDbRef (d x)) docs
        triples     = gr' $ map (\ref -> (locationToStr $ toM ref, unsetId $ toM ref, strToId $ idOf' ref)) refs
        toM x       = case x of
            DocTyped DTyped{typ="dbRef",val=DocMap m} -> m;
            otherwise -> error "Location should be a DMap." 
        unsetId x   = unset "id" x
        locationToStr x = dbRefToStr $ unsetId x
    nestedPairs <- mapM (toCollIdPairs db) triples
    let idcMap = M.fromList $ List.concat nestedPairs
        lookup x = case M.lookup x idcMap of Just z -> z; Nothing -> M.empty
        swapper :: DocVal -> DocVal
        swapper x = if isDbRef x
            then d . lookup $ T.concat [idOf' x, "-", locationToStr (toM x)]
            else x
    return $ map (\x -> docValToDoc (ma swapper (d x))) docs

e1 (a,_,_) = a
e2 (_,a,_) = a
e3 (_,_,a) = a

grBy :: Ord a1 => (a -> a1) -> [a] -> [[a]]
grBy f l = List.groupBy ((==) `F.on` f) $ List.sortBy (O.comparing f) l

-- | Same as gr but with a 3-tuple.
gr' :: Ord a => [(a, t, b)] -> [(a, t, [b])]
gr' l = map (\l -> (e1 . head $ l, e2 . head $ l, map e3 l)) $ grBy e1 l

-- |Groups a list of tuples by first element.
gr :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
gr = map (\l -> (fst . head $ l, map snd l)) . List.groupBy ((==) `F.on` fst) . List.sortBy (O.comparing fst)

{--------------------------------------------------------------------
  Server related.  
--------------------------------------------------------------------}

data Req = Req {
    path        :: [T.Text],    -- | Request path split by forward dashes.
    params      :: Document,    -- | Query params.
    cookies     :: Document,    -- | Cookies.
    languages   :: [T.Text],    -- | Accept languages sorted by priority.
    files       :: [T.Text]     -- | Absolute file pathes of uploaded files.
}

-- | A location is essentially a link, but with a structure.
-- ["cars", "#id"], fromList [("id", "4998a9a8sa8sa8s81")]
data Location = Location [T.Text] Document

-- | Splits a T.Text by forward dashes, and filters the empty ones.
-- "/cars/114/comments/9"   -> ["cars", "114", "comments", 9]
splitNorm :: T.Text -> [T.Text]
splitNorm x = filter (\x -> T.length x > 0) $ T.splitOn "/" x

parseLocation :: [T.Text] -> T.Text -> Maybe Location
parseLocation scheme t =
    let pairs = zip scheme $ splitNorm t
        tester (a,b) = if T.head a == '#'
            then Just (T.tail a, b)
            else if a == b
                then Just (a, b)
                else Nothing
        mPairs = sequence $ map tester pairs
    in case mPairs of
        Just mpr    -> Just $ Location scheme $ queryToDoc $ map (\(a, b) -> (TE.encodeUtf8 a, Just (TE.encodeUtf8 b))) mpr
        Nothing     -> Nothing

instance Show Location where
    show (Location a b) =
        let f x = if T.head x == '#'
            then case M.lookup (T.tail x) b of
                Just docv   -> case docv of
                    DocString s     -> s
                    DocFloat f      -> T.pack $ show f
                    DocInt i        -> T.pack $ show i
                    otherwise       -> T.pack $ show docv
                Nothing     -> error $ "Can't find element: " ++ show x
            else x
        in "/" ++ (T.unpack $ T.intercalate "/" $ map f a)

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

-- | Response content body.
data Body = Body {
    ctype       :: T.Text,              -- | content type
    content     :: LBS.ByteString       -- | actual content
}

-- | Response.
data Resp = Resp {
    status      :: Status,              -- | Status code.
    body        :: Body,                -- | Response body.
    store       :: Document,            -- | Cookies to store.
    unstore     :: [T.Text]             -- | Keys of cookies to delete.
}

-- | Configuration needed to start up the HTTP server.
data Config = Config {
    httpPort    :: Int,                 -- | Port to listen on (default: 8080)
    temp        :: T.Text               -- | Temporary folder to store uploaded files in.
}

defaultConfig :: Config
defaultConfig = Config 8080 "c:/temp"

{--------------------------------------------------------------------
  HTTP server implementation helpers.  
--------------------------------------------------------------------}

headersToDoc :: [HTypesHeader.Header] -> Document
headersToDoc h = queryToDoc $ trans h where
    trans x = map (\(key, val) -> (CI.original key, Just val)) x

-- docToHeaders :: Document -> [HTypesHeader.Header]
-- docToHeaders d = List.concat $ map f d where
--     f x = case x of
--         DocString s     -> 

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

{--------------------------------------------------------------------
  Concrete HTTP server implementation (using Warp).  
--------------------------------------------------------------------}

waiToFrame :: Wai.Request -> Req
waiToFrame wr = Req (Wai.pathInfo wr) (queryToDoc $ Wai.queryString wr) emptyDoc [] []

frameToWai :: Cond.ResourceT IO Resp -> Cond.ResourceT IO Wai.Response
frameToWai fresp = do
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
        mimeType = "text/html" -- Ma.findWithDefault "text/html" (ctype $ body fr) EM.extToMimeType
        contentType = ("Content-Type", mimeType)
        cookies = []
    return $ Wai.responseLBS statusCode [] (content $ body fr)

-- | Start the server. Example:
-- > startServer defaultConfig (\req -> return $ Resp OK (Body "text/html" "Hello.") emptyDoc [])
startServer :: Config -> (Req -> IO Resp) -> IO ()
startServer conf reqHandler = do
    let p = httpPort conf
    putStrLn $ "Server started listening on port " ++ show p
    Warp.run p (\a -> frameToWai $ liftIO $ reqHandler (waiToFrame a))