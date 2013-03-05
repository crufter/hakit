{-# LANGUAGE OverloadedStrings #-}

module Hakit.Db (
    Db(..),
    Specials(..),
    specials,
    docToSpecials,
    toDocStyleSort
) where

import Hakit
import qualified Data.List as List
import qualified Data.Function as F
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Ord as O

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

-- | 
data Specials = Specials {
    sort                :: [T.Text],    -- Example: ["x", "-y"]
    limit, skip, page   :: Integer,
    loc                 :: Document         -- Example: ["server" .- "main", "db" .- "testDb", "coll" .- "testCollection"]
}

-- ["x", "-y"] -> ["x" .- 1, "y" .- (-1)]
toDocStyleSort :: [T.Text] -> Document
toDocStyleSort a =
    let f x = if T.isInfixOf "-" x
            then (T.tail x) .- (-1::Integer)
            else x .- (1::Integer)
    in M.fromList $ map f a

docToSpecials doc =
    let spec = fst (specials doc)   -- Just to decrease algorightmic complexity.
        sort = dvToStrList (get "sort" spec)
        lim = dvToI (get "limit" spec)
        skip = dvToI (get "skip" spec)
        page = dvToI (get "page" spec)
        loc = dvToM (get "location" spec)
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
    in Specials sort lim skip page loc

{--------------------------------------------------------------------
  A farily simple database class.  
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

{--------------------------------------------------------------------
  Implementation of the resolve method.  
--------------------------------------------------------------------}    

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
            otherwise -> error "Location should be a Document." 
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

grBy :: Ord a1 => (a -> a1) -> [a] -> [[a]]
grBy f l = List.groupBy ((==) `F.on` f) $ List.sortBy (O.comparing f) l

-- | Same as gr but with a 3-tuple.
gr' :: Ord a => [(a, t, b)] -> [(a, t, [b])]
gr' l = map (\l -> (e1 . head $ l, e2 . head $ l, map e3 l)) $ grBy e1 l