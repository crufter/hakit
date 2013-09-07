{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}

{-|

This module contains the core type definitions, class instances, and utility functions
on them.

-}

module Hakit (
    -- * Document related types.
    Document,
    DList,
    DTyped(..),
    DocVal(..),
    DocValComp(..),
    DocComp(..),
    -- * Convenience functions.
    isInt,
    toInt,
    isFloat,
    toFloat,
    isString,
    toString,
    isBool,
    toBool,
    isMap,
    toMap,
    isList,
    toList,
    isNil,
    toNil,
    len,
    getString,
    getInt,
    getFloat,
    getBool,
    getMap,
    getList,
    getNil,
    isLeft,
    isRight,
    d,
    dt,
    dm,
    (.-),
    -- * Operations on documents.
    get,
    exists,
    set,
    unset,
    filt,
    flatten,
    ma,
    -- * JSON support.
    fromJSON,
    toJSON,
    -- * Other.
    nilDoc,
    e1,
    e2,
    e3,
    gr,
    Location(..),
    interpretDoc,
    interpretDoc'
) where

import qualified Data.List as List
import qualified Data.List.Split as Spl
import qualified Safe
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Function as F
import qualified Data.Ord as O
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TEL

-- JSON functions
import qualified Data.Aeson as J
import qualified Data.Attoparsec.Number as AP
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

{--------------------------------------------------------------------
  Document.  
--------------------------------------------------------------------}

type Document = M.Map T.Text DocVal
type DList = [DocVal]

-- Allows us to store some metainformation along with the DocVal.
data DTyped = DTyped {
    typ :: T.Text,
    val :: DocVal
} deriving (Eq, Ord, Show)

data DocVal 
    =   DocInt          Integer
    |   DocFloat        Double
    |   DocString       T.Text
 -- |   DocBS           ByteString
    |   DocBool         Bool
    |   DocMap          Document
    |   DocList         DList
    |   DocTyped        DTyped     
    |   Nil
    deriving (Ord, Eq, Show)

isInt a =       case a of
    DocInt b -> True
    otherwise -> False

toInt a =       case a of
    DocInt b -> b
    otherwise -> error $ show a ++ " is not an Integer."

isFloat a =     case a of
    DocFloat b -> True
    otherwise -> False

toFloat a =     case a of
    DocFloat b -> b
    otherwise -> error $ show a ++ " is not a Float."

isString a =    case a of
    DocString b -> True
    otherwise -> False

toString a =    case a of
    DocString b -> b
    otherwise -> error $ show a ++ " is not a String (Text)."

isBool a =      case a of
    DocBool b -> True
    otherwise -> False

toBool a =      case a of
    DocBool b -> b
    otherwise -> error $ show a ++ " is not a Bool."

isMap a =       case a of
    DocMap b -> True
    otherwise -> False

toMap a =       case a of
    DocMap b -> b
    otherwise -> error $ show a ++ " is not a Map."

isList a =      case a of
    DocList b -> True
    otherwise -> False

toList a =      case a of
    DocList b -> b
    otherwise -> error $ show a ++ " is not a List."

isNil a =       case a of
    Nil -> True
    otherwise -> False

toNil a =       case a of
    Nil -> Nil
    otherwise -> error $ show a ++ " is not a Nil."

len a = case a of
    DocString s -> T.length s
    DocList l   -> length l
    DocMap m    -> M.size m
    otherwise   -> error $ "len applied on incompatible DocVal: " ++ show a

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

instance DocValComp Document where
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

-- | Converts a compatible type into a DocVal.
d :: DocValComp a => a -> DocVal
d a = toDocVal a

-- | Shorthand to create a DocTyped value.
dt :: DocValComp a => T.Text -> a -> DocVal
dt typ val = DocTyped $ DTyped typ $ toDocVal val

infix 0 .-
-- | Helps to easily create a document (compatible type), like ["name" .- "Joey"]
(.-) :: DocValComp b => T.Text -> b -> (T.Text, DocVal)
(.-) a b = (a, toDocVal b)

isLeft :: Either a b -> Bool
isLeft a = case a of
    Left a -> True
    Right a -> False

isRight :: Either a b -> Bool
isRight a = not $ isLeft a

{--------------------------------------------------------------------
  Document operations.  
--------------------------------------------------------------------}

-- One level of access path.
data AccElem 
    =   AccStr T.Text
    |   AccInt Integer deriving (Show)

get' :: AccElem -> DocVal -> (DocVal, Bool)
get' a b = let notFound = (Nil, False) in case a of
    AccStr s    -> case b of
        DocMap m    -> case M.lookup s m of
            Just dv -> (dv, True)
            Nothing -> notFound
        otherwise   -> notFound
    AccInt i    -> case b of
        DocList l   -> if (length l) > (fromIntegral i)
            then (Safe.atNote "at get'" l (fromIntegral i), True)
            else notFound
        otherwise   -> notFound

parseAccElems :: T.Text -> [AccElem]
parseAccElems a = map parseOne parts where
    unp = T.unpack a 
    parts = Spl.split ((Spl.dropBlanks . Spl.dropDelims . Spl.oneOf) ".[]") unp
    parseOne a = let x = Safe.readMay a in
        case x of
            Just x      -> AccInt (toInteger x)
            Nothing     -> AccStr $ T.pack a

unparseAccElems :: [AccElem] -> T.Text
unparseAccElems xs = T.intercalate "." $ map f xs where
    f x = case x of
        AccStr s    -> s
        AccInt i     -> T.pack $ show i

getRec :: T.Text -> Document -> (DocVal, Bool)
getRec path doc = getRecurs accElems (DocMap doc) where
    accElems = parseAccElems path
    getRecurs :: [AccElem] -> DocVal -> (DocVal, Bool)
    getRecurs elems docval
        | length elems == 1     = get' (Safe.atNote "getRec 1" elems 0) docval
        | length elems > 1      = getRecurs (tail elems) $ fst $ get' (Safe.atNote "getRec 2" elems 0) docval

-- | Get element with dot notation, eg:
-- > get "author.books[1].title" example
-- Returns nil if the value specified by the path is Nil or that path is nonexistent.
-- To differentiate between Nils and nonexistent access pathes see the exists method.
get :: T.Text -> Document -> DocVal
get path doc = fst $ getRec path doc

-- Helper functions for fast retrieval when you are sure the element is present.
getString   a b = case get a b of
    DocString s   -> s
    otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a String (Text)."

getInt      a b = case get a b of
    DocInt i      -> i
    otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not an Int."

getFloat    a b = case get a b of
    DocFloat f    -> f
    otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a Float."

getBool     a b = case get a b of
    DocBool b     -> b
    otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a Bool."

getList     a b = case get a b of
    DocList l     -> l
    otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a List."

getMap      a b = case get a b of
    DocMap m      -> m
    otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a Map."

getNil      a b = case get a b of
    Nil           -> Nil
    otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a Nil."

getTyped    a b = case get a b of
    DocTyped t    -> t
    otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not Typed."

-- | Returns true if the element specified by the path exists.
-- Supports the same dot notation as get.
exists :: T.Text -> Document -> Bool
exists path doc = snd $ getRec path doc

-- | Sets an element specified by path.
-- Supports the same dot notation as get.
-- If the path is nonexistent, it will create the maps along the way, but will not grow any lists.
-- If the path already exists, it will replace the element.
set :: DocValComp d => T.Text -> d -> Document -> Document
set key val doc = fst $ set' key val doc

isAccInt x = case x of
    AccInt _    -> True
    otherwise   -> False

-- Returns the possibly updated Document and a Bool indicating wether
-- the document has been changed.
set' :: DocValComp d => T.Text -> d -> Document -> (Document, Bool)
set' key val doc = (toMap a, b)
    where
    accElems = parseAccElems key
    (a, b) = setRecurs accElems (d val) $ d doc
    replace :: Int -> a -> [a] -> [a]
    replace w value lst = if length lst <= w || length lst == 0
        then error "bug at replace call site"
        else if w == length lst - 1
            then init lst ++ [value]
            else let (x, _:ys) = List.splitAt w lst in x ++ value : ys
    lists :: [Int]
    lists = List.findIndices isAccInt accElems
    allLen = length accElems
    modMap :: (DocVal, Bool) -> T.Text -> Document -> (DocVal, Bool)
    modMap (a, modded) s d1 = if modded
        then (DocMap $ M.alter (\_ -> Just $ a) s d1, True)
        else (DocMap d1, False)
    setRecurs :: [AccElem] -> DocVal -> DocVal -> (DocVal, Bool)
    setRecurs elems newVal v
        | length elems == 1     = case v of
            DocMap m    -> case elems!!0 of
                AccStr s    -> (DocMap $ M.alter (\_ -> Just $ newVal) s m, True)
                otherwise   -> error "got map, wanted list"
            DocList l   -> case elems!!0 of
                AccInt i    -> let ix = (fromInteger i)::Int in
                    if length l <= ix || length l == 0
                        then (v, False)
                        else (DocList $ replace ix newVal l, True)
                otherwise   -> error "got list, wanted map"
            otherwise   -> (v, False)
        | length elems > 1      = let e = elems!!0 in case e of
            AccStr s    -> case v of
                DocList l   -> (v, False)
                DocMap m    -> case M.lookup s m of
                    Just x  -> modMap (setRecurs (tail elems) newVal $ d x) s m
                    Nothing -> if any (> (allLen - length elems - 1)) lists
                        then (v, False)
                        else modMap (setRecurs (tail elems) newVal $ d (M.empty::M.Map T.Text DocVal)) s m
                otherwise   -> (v, False)
            AccInt i    -> case v of
                DocMap m    -> (v, False)
                DocList l   -> let ix = (fromInteger i)::Int in
                    if length l <= ix || length l == 0
                        then (v, False)
                        else let (mv, modded) = setRecurs (tail elems) newVal $ l!!ix in
                            if modded
                                then (DocList $ replace ix mv l, True)
                                else (v, False)

-- | Unsets an element in a map specified by path.
-- Supports the same dot notation as get.
unset :: T.Text -> Document -> Document
unset key doc = fst $ unset' key doc

unset' :: T.Text -> Document -> (Document, Bool)
unset' key doc = let ae1 = parseAccElems key in f ae1
    where
    f ae
        | length ae == 0      = (doc, False)
        -- Can't unset a list.
        | isAccInt $ last ae  = (doc, False)
        | length ae == 1      = case M.lookup key doc of
            Just x  -> (M.alter (\_ -> Nothing) key doc, True)
            Nothing -> (doc, False)
        | otherwise           =
            let p = unparseAccElems $ init ae
                (v, ex) = getRec p doc
            in if not ex
                then (doc, False)
                else case v of
                    DocMap m    -> let p1 = unparseAccElems [last ae] in case M.lookup p1 m of
                        Just x  -> (set p (M.alter (\_ -> Nothing) p1 m) doc, True)
                        Nothing -> (doc, False)
                    otherwise   -> (doc, False)

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

-- | An empty document.
nilDoc :: M.Map T.Text DocVal
nilDoc = M.empty

size a = M.size a

-- | A typeclass for types convertible to a Document.
class DocComp a where
    toDoc :: a -> Document

instance DocComp [(T.Text, DocVal)] where
    toDoc = M.fromList

instance DocComp Document where
    toDoc = id

instance DocValComp dc => DocComp (M.Map T.Text dc) where
    toDoc v = dm . map (\(k, val) -> (k, toDocVal val)) $ M.toList v

dm :: DocComp d => d -> Document
dm x = toDoc x

{--------------------------------------------------------------------
  JSON support.  
--------------------------------------------------------------------}

j2d :: J.Value -> DocVal
j2d j =
    case j of
        J.Object a  -> DocMap . dm . map (\(k, v) -> (k, j2d v)) $ HM.toList a
        J.Array a   -> DocList $ map j2d $ V.toList a
        J.String a  -> DocString a
        J.Number a  -> case a of
            AP.I b   -> DocInt b
            AP.D b   -> DocFloat b
        J.Bool a    -> DocBool a
        J.Null      -> Nil

fromJSON :: T.Text -> Document
fromJSON t =
    let r = fromJSON' t
    in case r of
        Nothing     -> error $ "Unsuccesful decode: " ++ show t
        Just a      -> a
    

fromJSON' :: T.Text -> Maybe Document
fromJSON' t =
    let bs = LBS.fromStrict $ TE.encodeUtf8 t
        mVal = (J.decode bs)::Maybe J.Object
    in case mVal of
        Nothing -> Nothing
        Just a  -> Just . dm . map (\(k, v) -> (k, j2d v)) $ HM.toList a

-- | Would be much better to have a generic
-- Maybe a -> (a, Bool), but I don't know how to get
-- zero value of any type. TOOD: look that up.
fromJSON'' :: T.Text -> (Document, Bool)
fromJSON'' t =
    let r = fromJSON' t
    in case r of
        Nothing     -> (nilDoc, False)
        Just a      -> (a,      True)

d2j :: DocVal -> J.Value
d2j d =
    case d of
        DocMap a    -> J.Object . HM.fromList . map(\(k, v) -> (k, d2j v)) $ M.toList a
        DocList a   -> J.Array . V.fromList $ map d2j a
        DocString a -> J.String a
        DocInt a    -> J.Number $ AP.I a
        DocFloat a  -> J.Number $ AP.D a
        DocBool a   -> J.Bool a
        Nil         -> J.Null 

toJSON :: Document -> T.Text
toJSON d =
    let jObj = J.Object . HM.fromList . map (\(k, v) -> (k, d2j v)) $ M.toList d
    in TE.decodeUtf8 . LBS.toStrict $ J.encode jObj

{--------------------------------------------------------------------
  Other.  
--------------------------------------------------------------------}

e1 (a,_,_) = a
e2 (_,a,_) = a
e3 (_,_,a) = a

-- | Groups a list of tuples by first element.
gr :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
gr = map (\l -> (fst . head $ l, map snd l)) . List.groupBy ((==) `F.on` fst) . List.sortBy (O.comparing fst)

-- | A location is essentially a link, but with a structure.
-- ["cars", "#id"], fromList [("id", "4998a9a8sa8sa8s81")]
data Location = Location [T.Text] Document

instance Show Location where
    show (Location a b) =
        "/" ++ case a of
            []      -> ""
            (y:ys)  ->
                let f x = if T.length x == 0
                        then ""
                        else if T.head x == '#'
                            then case M.lookup (T.tail x) b of
                                Just docv   -> case docv of
                                    DocString s     -> s
                                    DocFloat f      -> T.pack $ show f
                                    DocInt i        -> T.pack $ show i
                                    otherwise       -> T.pack $ show docv
                                Nothing     -> error $ "Can't find element: " ++ show x
                            else x
                in T.unpack . T.intercalate "/" $ map f a

-- | Creates a Document out of a list of key value pairs.
-- Tries to read bools, nils, floats, ints, and creates lists out of duplicate elements.
-- Maps are not supported yet.
interpretDoc :: [(BS.ByteString, Maybe BS.ByteString)] -> Document
interpretDoc q = M.fromList $ map singlify (gr (map f q)) where
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

interpretDoc' :: [(T.Text, T.Text)] -> Document
interpretDoc' q = interpretDoc $ map (\(a, b) -> (TE.encodeUtf8 a, Just $ TE.encodeUtf8 b)) q