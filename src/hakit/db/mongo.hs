{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Hakit.Db.Mongo (
    Db(..),
    new
) where

import Control.Monad (liftM)
import qualified Database.MongoDB as Mdb
import qualified Hakit
import qualified Hakit.Db as HD
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.List.Split as Spl
-- import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.Word as W
import qualified Data.List as L
import qualified Numeric as N
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Safe

instance Show Mdb.Pipe where
    show p = "Mdb.Pipe"

data Db = Db {
    -- M.fromList [("127.0.0.1:27017", pipe1), ("example.com:27017", pipe2)]
    connections :: M.Map T.Text Mdb.Pipe,
    -- ["server" Hakit..- "127.0.0.1:27017", serverName Hakit..- "main", "db" Hakit..- "testDb"]
    location    :: Hakit.Document
} deriving (Show)

new = Db (M.fromList []) M.empty

readH x = fst (Safe.atNote "readH" (N.readHex x) 0)
-- Hex string to (Oid Word32 Word64)
strToOid x = let (f, s) = L.splitAt 8 (T.unpack x) in Mdb.Oid ((fromIntegral (readH f))::W.Word32) ((fromIntegral (readH s))::W.Word64)

-- Such an ugly hack, but it seems the mongo driver does not support dbRefs.
-- Temporal solution (yeah, sure... as always).
marker :: T.Text
marker = ">'" -- Marks the beginning of a string containing a dbRef.

fromDocVal :: Hakit.DocVal -> Mdb.Value
fromDocVal v = case v of
    Hakit.DocTyped      t   -> case t of
        Hakit.DTyped{Hakit.typ="id", Hakit.val=Hakit.DocString s}       -> Mdb.ObjId $ strToOid s
        Hakit.DTyped{Hakit.typ="dbRef", Hakit.val=Hakit.DocMap m}       -> Mdb.String  $ T.append marker (HD.dbRefToStr m)
    Hakit.DocInt        i   ->  Mdb.Int64 (fromInteger i)
    Hakit.DocFloat      f   ->  Mdb.Float f
    Hakit.DocString     s   ->  if T.isPrefixOf marker s then error "dbRef marker found in DocString" else Mdb.String s
    Hakit.DocBool       b   ->  Mdb.Bool b
    Hakit.DocMap        m   ->  Mdb.Doc $ map fromDocField (M.toList m)
    Hakit.DocList       l   ->  Mdb.Array $ map fromDocVal l
    Hakit.Nil               ->  Mdb.Null

instance Mdb.Val Mdb.Value where
    val a = a
    cast' a = Just a

fromDocField :: (T.Text, Hakit.DocVal) -> Mdb.Field
fromDocField (a, b) = case a of
    "id"        -> case b of
        Hakit.DocString s                                                           -> "_id" Mdb.=: (strToOid s)
        Hakit.DocTyped Hakit.DTyped{Hakit.typ="id", Hakit.val=Hakit.DocString s}    -> "_id" Mdb.=: (strToOid s)
    otherwise   -> a Mdb.=: (fromDocVal b)

fromDoc :: Hakit.Document -> Mdb.Document
fromDoc a = map fromDocField (M.toList a)

uToB :: Mdb.UUID -> S.ByteString
uToB x = case x of Mdb.UUID bs -> bs

strToDbRef str = HD.strToDbRef . snd $ T.splitAt 2 str

toDocVal :: Mdb.Value -> Hakit.DocVal
toDocVal v = case v of
    Mdb.Uuid        uuid    -> Hakit.dt "id" $ T.pack . BS.unpack $ uToB uuid
    Mdb.ObjId       objId   -> error "Got Mdb.ObjId, expected Mdb.Uuid"  -- How come we send in objectIds, but get back uuids?
    Mdb.String      s       -> if T.isPrefixOf marker s then strToDbRef s else Hakit.DocString s
    Mdb.Int64       i       -> Hakit.DocInt (toInteger i)
    Mdb.Float       f       -> Hakit.DocFloat f
    Mdb.Bool        b       -> Hakit.DocBool b
    Mdb.Array       a       -> Hakit.DocList $ map toDocVal a
    Mdb.Doc         d       -> Hakit.DocMap $ M.fromList $ map toDocField d
    otherwise               -> Hakit.Nil

idField :: Mdb.Field -> (T.Text, Hakit.DocVal)
idField x = case (Mdb.value x) of
    Mdb.ObjId    oid -> ("id", Hakit.dt "id" (T.pack $ show oid))

toDocField :: Mdb.Field -> (T.Text, Hakit.DocVal)
toDocField a = let x = T.unpack (Mdb.label a) in case x of
    "_id"               -> idField a
    otherwise           -> (T.pack x, toDocVal (Mdb.value a))

toDoc :: Mdb.Document -> Hakit.Document
toDoc a = M.fromList $ map toDocField a

fromRight x = case x of
    Left l  -> error (show l)
    Right r -> r

safeDec x = if x > 1 then x - 1 else 0
stripDoc doc = Hakit.unset "location" doc
genSel hakitDoc coll = Mdb.select (fromDoc (stripDoc hakitDoc)) coll
-- find, findOne
genQuery :: Hakit.Document -> T.Text -> Mdb.Query
genQuery hakitDoc coll =
    let (specDoc, doc) = HD.specials hakitDoc
        spec = HD.docToSpecials specDoc
        select = Mdb.select (fromDoc doc) coll
        setSort sel = if length (HD.sort spec) > 0
            then sel{Mdb.sort=(fromDoc (HD.toDocStyleSort (HD.sort spec)))}
            else sel
        setLimit sel = if HD.limit spec > 0
            then sel{Mdb.limit=(fromIntegral (HD.limit spec)::W.Word32)}
            else sel
        setSkip sel = if HD.skip spec > 0
            then sel{Mdb.skip=(fromIntegral (HD.skip spec)::W.Word32)}
            else if HD.page spec > 0
                then sel{Mdb.skip=(fromIntegral (HD.page spec)::W.Word32) * safeDec (fromIntegral (HD.limit spec)::W.Word32)}
                else sel
        in
    (setSort . setLimit . setSkip) select

merge def new = M.union new def
detLocation defLoc origiDoc = let loc = HD.loc $ HD.docToSpecials (fst (HD.specials origiDoc)) in
    merge defLoc loc
getColl :: Db -> Hakit.Document -> T.Text
getColl db doc = let (spec, _) = HD.specials doc in Hakit.getString "coll" $ detLocation (location db) spec
fromJust x = case x of Just l -> l; otherwise -> error "Can't convert from Just."

runAction (Db conns def) origiDoc action =
    let loc = detLocation def origiDoc
        pip = fromJust $ M.lookup (Hakit.getString "server" loc) conns
        dbName = Hakit.getString "db" loc in
    Mdb.runIOE $ Mdb.access pip Mdb.master dbName action
find' db sel = do
    r <- runAction db sel $ Mdb.rest =<< Mdb.find (genQuery sel (getColl db sel))
    return $ ((map toDoc) . fromRight) r
update' db sel mod = do
    r <- runAction db sel $ Mdb.modify (genSel sel (getColl db sel)) (fromDoc mod)
    return $ fromRight r

instance HD.Db Db where
    location db = M.empty
    connect (Db conns loc) m2 = do
        let m1 = Hakit.toDoc m2
            server = Hakit.getString "server" m1
            host = let p = T.splitOn ":" server in if length p == 2
                then Mdb.readHostPort (T.unpack server)
                else Mdb.host (T.unpack server)
            authMode = Hakit.exists "user" m1
        pip <- Mdb.runIOE $ Mdb.connect $ host
        if authMode
            then do
                let dbName      = if Hakit.exists "db" m1
                        then Hakit.getString "db" m1
                        else error "Trying to establish authenticated connection, but \"db\" is not given."
                    user        = Hakit.getString "user" m1
                    password    = Hakit.getString "password" m1
                authSuc <- Mdb.runIOE $ Mdb.access pip Mdb.master dbName $ Mdb.auth user password
                if fromRight authSuc
                    then return ()
                    else error $ show $ T.concat ["Could not authenticate user ", user, " against ", server]
            else return ()
        let m = if Hakit.exists "serverName" m1
                then Hakit.unset "serverName" (Hakit.set "server" (Hakit.get "serverName" m1) m1)
                else m1
            locFrom old new = if M.size old >= 3
                then old
                else M.union old new
            newLoc = locFrom loc m
        return $ Db (M.insert (Hakit.getString "server" m) pip conns) newLoc
    dropDb (Db conns def) locDoc = do
        let loc = merge def $ Hakit.toDoc locDoc
            dbName = Hakit.getString "db" loc
            serverName = Hakit.getString "server" loc
            pip = fromJust $ M.lookup serverName conns
        Mdb.runIOE $ Mdb.access pip Mdb.master dbName (Mdb.dropDatabase dbName)
        return ()
    dropCurrent db = HD.dropDb db (M.empty::M.Map T.Text Hakit.DocVal)
    servers db = []
    setLocation (Db conns loc) doc = Db conns $ M.union (Hakit.toDoc doc) loc
    insert db doc1 = do
        let doc = Hakit.toDoc doc1
        id <- liftM (T.pack . show) (Mdb.genObjectId)
        runAction db doc $ Mdb.insert (getColl db doc) (fromDoc (stripDoc doc))
        return id
    insertAll db docs = do
        -- runAction db $ Mdb.insertAll (T.pack (coll db)) (map fromDoc docs)
        return []
    find db sel = find' db $ Hakit.toDoc sel
    findOne db sel = do
        sels <- find' db $ Hakit.toDoc sel
        if length sels == 0
            then return Nothing
            else return $ Just (Safe.atNote "findOne" sels 0)
    count db sel1 = do
        let sel = Hakit.toDoc sel1
        c <- runAction db sel $ Mdb.count (genSel sel (getColl db sel))
        return $ (toInteger . fromRight) c
    replace db sel1 doc1 = do
        let sel = Hakit.toDoc sel1
            doc = Hakit.toDoc doc1
        r <- runAction db sel $ Mdb.replace (genSel sel (getColl db sel)) (fromDoc doc)
        return $ fromRight r
    update db sel mod = update' db (Hakit.toDoc sel) (Hakit.toDoc mod)
    delete db sel1 = do
        let sel = Hakit.toDoc sel1
        r <- runAction db sel $ Mdb.delete (genSel sel (getColl db sel))
        return $ fromRight r
    deleteOne db sel1 = do
        let sel = Hakit.toDoc sel1
        r <- runAction db sel $ Mdb.deleteOne (genSel sel (getColl db sel))
        return $ fromRight r
    iterate db sel1 f = do
        let sel = Hakit.toDoc sel1
        cursor <- runAction db sel $ Mdb.find (genQuery sel (getColl db sel))
        let consume = do
            r <- runAction db sel $ Mdb.nextBatch (fromRight cursor)
            if length (fromRight r) == 0
                then return ()
                else do
                    mapM_ f (map toDoc (fromRight r))
                    consume
        consume