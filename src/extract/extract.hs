module Hframe.Extract (
    extract, extractSafe
) where

import Hframe.Frame
import qualified Data.List as L
import Control.Monad.Error
import qualified Control.Monad as M

type Validator = String -> DocVal -> Document -> Either String DocVal

builtins = [
    ("int", inter),
    ("float", floater),
    ("string", stringer),
    ("bool", booler),
    ("const", conster)
    ]

minMaxCheck key min max dat =
    if (min /= Nil && not (isInt min)) || (max /= Nil && not (isInt max))
        then Left $ "min or max of " ++ key ++ " rules must be Frame.Nil or Frame.DocInt"
        else Right dat

-- Min and max check for floats.
minMaxCheck' key min max dat =
    if (min /= Nil && not (isFloat min)) || (max /= Nil && not (isFloat max))
        then Left $ "min or max of " ++ key ++ " rules must be Frame.Nil or Frame.DocInt"
        else Right dat

inter :: Validator
inter key dat rule = do
    let min = get "min" rule
        max = get "max" rule
    minMaxCheck key min max dat
    if not (isInt dat)
       then Left $ key ++ " is not an int."
       else Right dat
    if min /= Nil && min > dat
        then Left $ "int " ++ key ++ " too small."
        else Right dat
    if max /= Nil && max < dat
        then Left $ "int " ++ key ++ " too large."
        else Right dat

floater :: Validator
floater key dat rule = do
    let min = get "min" rule
        max = get "max" rule
    minMaxCheck' key min max dat
    if not (isFloat dat)
        then Left $ key ++ " is not a float."
        else Right dat
    if min /= Nil && min > dat
        then Left $ "float " ++ key ++ " too small."
        else Right dat
    if max /= Nil && max < dat
        then Left $ "float " ++ key ++ " too large."
        else Right dat

-- Automatically converts anything to a string if that is not a string.
-- It is inconsistent with the other builtins, because they don't attempt convert, however, it is consistent with Frame, which tries to parse every string
-- into an Integer, Double, Bool, Nil in queryToDoc.
stringer :: Validator
stringer key dat rule = do
    let min = get "min" rule
        max = get "max" rule
    minMaxCheck key min max dat
    if not (isString dat)
        then Right $ d (show dat)   -- We convert to string here. Maybe we should only convert specific types.
        else Right dat
    if min /= Nil && min > (DocInt (fromIntegral (len dat)))
        then Left $ "string " ++ key ++ " is too short."
        else Right dat
    if max /= Nil && max < (DocInt (fromIntegral (len dat)))
        then Left $ "string " ++ key ++ " is too long."
        else Right dat

booler :: Validator
booler key dat rule =
    if not (isBool dat)
        then Left $ key ++ " is not a bool."
        else Right dat

conster :: Validator
conster key dat rule = Right $ get "value" rule
    
getValidator :: String -> [(String, Validator)] -> Validator
getValidator typ validators = case L.find (\(a, _) -> a == typ) validators of
    Just (_, v)  -> v
    Nothing -> error $ "Can not find validator for type " ++ typ

validateVal :: DocVal -> (String, Document) -> [(String, Validator)] -> Either String DocVal
validateVal docVal (key, rule) validators =
    let validator = getValidator (getString "type" rule) validators
    in
    case validator key docVal rule of
        Left x  -> Left x
        Right x -> Right x

extractSafe :: Document -> Document -> Either String Document
extractSafe doc rules =
    mapM (\r@(key, rule) ->
        if get "isList" rule == d True
            then docMapToList key (mapM (\e -> validateVal e r builtins) (toList (get key doc)))
            else M.liftM (\x -> (key, x)) (validateVal (get key doc) r builtins))
        (map transFormR rules)
    where
    toList :: DocVal -> [DocVal]
    toList d = case d of DocList l -> l; otherwise -> [d]
    
    docMapToList :: String -> Either String [DocVal] -> Either String (String, DocVal)
    docMapToList key list = case list of
        Left e      -> Left e
        Right l     -> Right $ (key, DocList l)
    
    transFormR :: (String, DocVal) -> (String, Document)
    transFormR a = case  a of
        (key, DocMap m)     -> if get "type" m == Nil then (key, set "type" m "string") else (key, m)
        (key, DocBool b)    -> (key, ["type" .- "string"])
        otherwise           -> error $ "Can't interpret rule: " ++ show a

fromRight :: Either String b -> b
fromRight x = case x of
    Left l  -> error l
    Right r -> r

-- First is subject, second is rules.
-- The document we are extracting from is meant to be flat, with the possible exception of a first level array.
extract :: Document -> Document -> Document
extract doc rules = fromRight $ extractSafe doc rules