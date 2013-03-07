{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

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
    -- * Other
    nilDoc,
    e1,
    e2,
    e3,
    gr,
    -- * Operations on documents.
    get,
    exists,
    set,
    unset,
    filt,
    flatten,
    ma,
    -- * Other.
    Location(..)
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

{--------------------------------------------------------------------
  Document.  
--------------------------------------------------------------------}

type Document = M.Map T.Text DocVal
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
                | DocMap        Document
                | DocList       DList
                | DocTyped      DTyped     
                | Nil
                deriving (Ord, Eq, Show)

isInt a =       case a of DocInt b -> True; otherwise -> False
toInt a =       case a of DocInt b -> b; otherwise -> error $ show a ++ " is not an Integer."
isFloat a =     case a of DocFloat b -> True; otherwise -> False
toFloat a =     case a of DocFloat b -> b; otherwise -> error $ show a ++ " is not a Float."
isString a =    case a of DocString b -> True; otherwise -> False
toString a =    case a of DocString b -> b; otherwise -> error $ show a ++ " is not a String (Text)."
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
getString   a b = case get a b of DocString s   -> s;   otherwise -> error $ (T.unpack a) ++ " in " ++ (show b) ++ " is not a String (Text)."
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
set key doc val = M.alter (\_ -> Just $ toDocVal val) key doc

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

dm :: DocComp d => d -> Document
dm x = toDoc x

{--------------------------------------------------------------------
  Other.  
--------------------------------------------------------------------}

e1 (a,_,_) = a
e2 (_,a,_) = a
e3 (_,_,a) = a

-- |Groups a list of tuples by first element.
gr :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
gr = map (\l -> (fst . head $ l, map snd l)) . List.groupBy ((==) `F.on` fst) . List.sortBy (O.comparing fst)

-- | A location is essentially a link, but with a structure.
-- ["cars", "#id"], fromList [("id", "4998a9a8sa8sa8s81")]
data Location = Location [T.Text] Document

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