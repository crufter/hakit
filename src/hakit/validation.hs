{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}

module Hakit.Validation (
    -- * Validation
    validate, validateSafe, builtins, Validator,
    -- * Misc
    listMin, listMax, listIgnore, min', max'
) where

import Hakit
import qualified Data.List as L
import Control.Monad.Error
import qualified Control.Monad as CM
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)

{--------------------------------------------------------------------
  Misc.  
--------------------------------------------------------------------}

-- Just to provide a little bit more type safety.
listMin =       "listMin"       :: T.Text
listMax =       "listMax"       :: T.Text
listIgnore =    "listIgnore"    :: T.Text
min' =          "min"           :: T.Text -- Bad name, but Prelude already has a min/max...
max' =          "max"           :: T.Text

{--------------------------------------------------------------------
  Validation.  
--------------------------------------------------------------------}

type Validator = T.Text -> DocVal -> Document -> Either String DocVal

-- | Builtin validators.
builtins :: [(T.Text, Validator)]
builtins = [
    ("int", inter),
    ("float", floater),
    ("string", stringer),
    ("bool", booler),
    ("const", conster)
    ]

minMaxCheck key min max dat =
    if (min /= Nil && not (isInt min)) || (max /= Nil && not (isInt max))
        then Left $ "min or max of " ++ (T.unpack key) ++ " rules must be Frame.Nil or Frame.DocInt"
        else Right dat

-- Min and max check for floats.
minMaxCheck' key min max dat =
    if (min /= Nil && not (isFloat min)) || (max /= Nil && not (isFloat max))
        then Left $ "min or max of " ++ (T.unpack key) ++ " rules must be Frame.Nil or Frame.DocInt"
        else Right dat

inter :: Validator
inter key dat rule = do
    let min = get "min" rule
        max = get "max" rule
    minMaxCheck key min max dat
    if not (isInt dat)
       then Left $ (T.unpack key) ++ " is not an int."
       else Right dat
    if min /= Nil && min > dat
        then Left $ "int " ++ (T.unpack key) ++ " too small."
        else Right dat
    if max /= Nil && max < dat
        then Left $ "int " ++ (T.unpack key) ++ " too large."
        else Right dat

floater :: Validator
floater key dat rule = do
    let min = get "min" rule
        max = get "max" rule
    minMaxCheck' key min max dat
    if not (isFloat dat)
        then Left $ (T.unpack key) ++ " is not a float."
        else Right dat
    if min /= Nil && min > dat
        then Left $ "float " ++ (T.unpack key) ++ " too small."
        else Right dat
    if max /= Nil && max < dat
        then Left $ "float " ++ (T.unpack key) ++ " too large."
        else Right dat

-- Automatically converts anything to a string if that is not a string.
-- It is inconsistent with the other builtins, because they don't attempt convert, however, it is consistent with Hakit, which tries to parse every string
-- into an Integer, Double, Bool, Nil in queryToDoc.
stringer :: Validator
stringer key dat rule = do
    let min = get "min" rule
        max = get "max" rule
    minMaxCheck key min max dat
    if not (isString dat)
        then Right $ d (T.pack $ show dat)   -- We convert to string here. Maybe we should only convert specific types.
        else Right dat
    if min /= Nil && min > (DocInt (fromIntegral (len dat)))
        then Left $ "string " ++ (T.unpack key) ++ " is too short."
        else Right dat
    if max /= Nil && max < (DocInt (fromIntegral (len dat)))
        then Left $ "string " ++ (T.unpack key) ++ " is too long."
        else Right dat

booler :: Validator
booler key dat rule =
    if not (isBool dat)
        then Left $ (T.unpack key) ++ " is not a bool."
        else Right dat

conster :: Validator
conster key dat rule = Right $ get "value" rule
    
getValidator :: T.Text -> [(T.Text, Validator)] -> Validator
getValidator typ validators = case L.find (\(a, _) -> a == typ) validators of
    Just (_, v) -> v
    Nothing     -> error $ "Can not find validator for type " ++ (T.unpack typ)

validateVal :: DocVal -> (T.Text, Document) -> [(T.Text, Validator)] -> Either String DocVal
validateVal docVal (key, rule) validators =
    let validator = getValidator (getString "type" rule) validators
    in case validator key docVal rule of
        Left x  -> Left x
        Right x -> Right x

data ListOpts = ListOpts {
    lmin, lmax      :: Int,
    lignore         :: Bool
} deriving (Show)

toListOpts :: Document -> ListOpts
toListOpts d =
    let lmin'       = get "listMin" d
        lmin        = if isInt lmin'
            then fromInteger $ toInt lmin'
            else 0
        lmax'       = get "listMax" d
        lmax        = if isInt lmax'
            then fromInteger $ toInt lmax'
            else maxBound::Int
        lignore'    = get "listIgnore" d
        lignore     = if isBool lignore'
            then toBool lignore'
            else False
    in ListOpts lmin lmax lignore

-- | Safe version of validate.
validateSafe :: Document -> Document -> Either String Document
validateSafe rules doc =
    CM.liftM dm . sequence . map fromJust . filter isJust $ map (\r@(key, rule) ->
        let subject = get key doc
            toPair :: a -> b -> (a, b)
            toPair key x = (key, x)
            ret :: Bool -> Either a b -> Maybe (Either a b)
            ret ignore x = if ignore && isLeft x
                then Nothing
                else Just x
        in if get "isList" rule == d True
            then
                let listOpts = toListOpts rule
                    eitherList'' = map (\e -> validateVal e r builtins) (toList subject)
                    eitherList' = if lignore listOpts
                        then filter isRight eitherList''
                        else eitherList''
                    eitherList = if length eitherList' < lmin listOpts || length eitherList' > lmax listOpts
                        then [Left $ T.unpack key ++ " length is not proper " ++ show eitherList' ++ show listOpts]
                        else eitherList'
                    verdict = CM.liftM (\x -> toPair key $ d x) $ sequence eitherList
                in ret (lignore listOpts) verdict
            else
                let ignore = if isBool $ get "ignore" rule
                        then toBool $ get "ignore" rule
                        else False
                    verdict = CM.liftM (toPair key) (validateVal subject r builtins)
                in ret ignore verdict
        )
        (map transformR $ Map.toList rules)
    where
    toList :: DocVal -> [DocVal]
    toList d = case d of
        DocList l -> l
        otherwise -> [d]
    -- Transform the rules so we dont have to deal with the irregular
    -- convenience formats at validation.
    transformR :: (T.Text, DocVal) -> (T.Text, Document)
    transformR a = case  a of
        (key, DocMap m)     -> if get "type" m == Nil
                                    then (key, set "type" "string" m)
                                    else (key, m)
        (key, DocBool b)    -> (key, dm ["type" .- "string"])
        otherwise           -> error $ "Can't interpret rule: " ++ show a

fromRight :: Either String b -> b
fromRight x = case x of
    Left l  -> error l
    Right r -> r

-- | Validate a given document.
-- First is rules, second is subject.
-- The document we are validating is meant to be flat, with the possible exception of a first level array.
validate :: Document -> Document -> Document
validate rules doc = fromRight $ validateSafe rules doc