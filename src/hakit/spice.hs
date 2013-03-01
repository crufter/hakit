{-# LANGUAGE OverloadedStrings #-}
module Hakit.Spice (
    -- * Tag creation
    tag, doctype, html, head', body, div, text,
    -- * Nested tag functions
    alter, remove, select,
    -- * Single tag functions
    attrs, attr, children, name, addClass, removeClass,
    -- * Types
    Attrs(), Tag(..), Child(..),
    -- * Exported for testing purposes only
    matches
) where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Text.ParserCombinators.Parsec as P

-- This package contains some questionable temporary names now to avoid clash with prelude.

{--------------------------------------------------------------------
  Types.  
--------------------------------------------------------------------}

-- For simplicity reasons, currently only attributes exist and no properties.
data Attrs = Attrs (M.Map T.Text T.Text) deriving (Eq)

toAttrs :: [(T.Text, T.Text)] -> Attrs
toAttrs l = Attrs $ M.fromList l

attrsToMap :: Attrs -> M.Map T.Text T.Text
attrsToMap (Attrs at) = at

instance Show Attrs where
    show (Attrs at) = concat $ map (\(a, b) -> T.unpack a ++ "=" ++ show b) $ M.toList at

type Child = Tag

data Tag =
        Doctype T.Text
    |   Text    T.Text  
    --          Name        Attributes  Children
    |   Tag     T.Text      Attrs       [Child]
    deriving (Eq)

-- | Create any tag.
tag n a c   = Tag n (toAttrs a) c

-- Some frequently used tags here
doctype a   = Doctype a
html a c    = tag "html" a c
head' a c   = tag "head" a c
body a c    = tag "body" a c
text t      = Text t
div' a c    = tag "div" a c

-- | Create attribute.
cat :: T.Text -> T.Text -> (T.Text, T.Text)
cat a b = (a, b)

example :: Tag
example =
    html [] [
        head' [] [],
        body [cat "style" "background: #ccc;"] [
            text "Hello world."
        ]
    ]

-- Show attributes
sa :: Attrs -> String
sa (Attrs at) = if M.size at > 0
        then " " ++ show at
        else ""

-- Show children.
sc :: [Child] -> String
sc x = L.intercalate "\n" $ map show x

instance Show Tag where
    show (Doctype a)    = "<!DOCTYPE " ++ T.unpack a ++ ">"
    show (Tag n a b)    = "<" ++ T.unpack n ++ sa a ++ ">" ++ sc b ++ "</" ++ T.unpack n ++ ">"
    show (Text a)       = T.unpack a

{--------------------------------------------------------------------
  Single tag functions.  
--------------------------------------------------------------------}

attrs :: Tag ->      Attrs
attrs (Doctype t)    = toAttrs []
attrs (Text t)       = toAttrs []
attrs (Tag _ a _)    = a

attr :: T.Text -> Tag -> Maybe T.Text
attr attrName tag = case M.lookup attrName $ attrsToMap $ attrs tag of
    Just v      -> Just v
    Nothing     -> Nothing

setAttr :: T.Text -> T.Text -> Tag -> Tag
setAttr key val tag = case tag of
    Doctype t           -> tag
    Text t              -> tag
    Tag n (Attrs a) c   -> Tag n (Attrs $ M.insert key val a) c

children :: Tag ->       [Child]
children (Doctype t)     = []
children (Text t)        = []
children (Tag _ _ c)     = c

name :: Tag ->      T.Text
name (Doctype t)    = "doctype"
name (Text t)       = "text"
name (Tag n _ _)    = n

addClass :: T.Text -> Tag -> Tag
addClass clas tag = case attr "class" tag of
    Nothing     -> setAttr "class" clas tag
    Just c      -> let spl = T.splitOn " " c in if elem clas spl
        then tag
        else setAttr "class" (T.intercalate " " $ clas:spl) tag

hasClass :: T.Text -> Tag -> Bool
hasClass clas tag = case attr "class" tag of
    Nothing     -> False
    Just c      -> let spl = T.splitOn " " c in elem clas spl

removeClass :: T.Text -> Tag -> Tag
removeClass clas tag = case attr "class" tag of
    Nothing     -> tag
    Just c      -> let spl = T.splitOn " " c in if elem clas spl
        then setAttr "class" (T.intercalate " " $ filter (/= clas) spl) tag
        else tag

toggleClass :: T.Text -> Tag -> Tag
toggleClass clas tag = case attr "class" tag of
    Nothing     -> setAttr "class" clas tag
    Just c      -> let spl = T.splitOn " " c in if elem clas spl
        then setAttr "class" (T.intercalate " " $ filter (/= clas) spl) tag
        else setAttr "class" (T.intercalate " " $ clas:spl) tag


{--------------------------------------------------------------------
  Selector implementation.  
--------------------------------------------------------------------}

-- | Just for quick and ugly testing.
-- Tests if a (top level) tag satisfies a given selector
-- (which can contain multiple criteria, like "div.className").
-- Obviously selectors like "div a" or "div > a" won't work in this case.
matches :: T.Text -> Tag -> Bool
matches sel tag = all (flip satisfiesSingle $ tag) (parseSelector sel)

parseSelector :: T.Text -> [Selector]
parseSelector t = let sels = P.parse parseExpr "selector" $ T.unpack t in
    case sels of
        Left e      -> error $ show e
        Right ss    -> ss

satisfiesSingle :: Selector -> Tag -> Bool
satisfiesSingle s tag = case s of
    Type t  -> name tag == t
    Id  id  -> case attr "id" tag of
        Just x  -> x == id
        Nothing -> False
    Class c -> case attr "class" tag of
        Just x  -> x == c
        Nothing  -> False
    Attribute reg attrName attrVal -> case attr attrName tag of
        Nothing -> False
        Just x  -> case reg of
            StartsWith  -> T.isPrefixOf attrVal x
            EndsWith    -> T.isSuffixOf attrVal x
            Contains    -> T.isInfixOf attrVal x
            Equals      -> x == attrVal
            Any         -> True
    Descendant      -> error $ "can't use descendant separator on: " ++ show tag
    DirectChild     -> error $ "can't use direct child separator on: " ++ show tag

-- Returns true if given tag satisfies the selector(s) provided.
-- Needs parents because of the " > " (direct child) and " " (descendant) relations.
satisfies :: [Tag] -> Tag -> [Selector] -> Bool
satisfies parents tag sels =
    let separator x = case x of
            Descendant  -> True
            DirectChild -> True
            otherwise   -> False
        nonseps = L.filter separator sels
        -- ps = Parents satisfy recursive
        -- Called with a sels list ending in a separator, and
        -- with sels' having even length. Eg: [selector, sep, selector, sep]
        ps :: [Tag] -> [Selector] -> Bool
        ps parents sels'
            | sels' == []       = True
            | parents == []     = False     -- And sels /= []
            | otherwise         =
                let sep = last sels'
                    crit = last . init $ sels'
                in case sep of
                    Descendant      -> if satisfiesSingle crit $ last parents
                        then ps (init parents) (init . init $ sels')
                        else ps (init parents) sels'
                    DirectChild     -> if satisfiesSingle crit $ last parents
                        then ps (init parents) (init . init $ sels')
                        else False
    -- Obviously if there are fewer parents than parent criterias, or the given tag does not
    -- satisfy the given criteria, we can stop.
    in if length parents < length nonseps - 1 || (not $ satisfiesSingle (last nonseps) tag)
        then False
        else if length sels == 1
            then True
            -- We only get here if sels has an odd length, length sels > 1 
            else ps parents $ init sels

{--------------------------------------------------------------------
  Nested tag functions.  
--------------------------------------------------------------------}

-- | Apply function on elements matching the selector.
alter :: T.Text -> Tag -> (Tag -> Tag) -> Tag
alter sel t f =
    let sels = parseSelector sel
        alterRec :: [Tag] -> Tag -> Tag
        alterRec parents tag = case tag of
            Doctype t       -> appif tag
            Text t          -> appif tag
            Tag n a c       -> appif $ Tag n a $ map (alterRec $ parents ++ [tag]) c
            where
                appif t =
                    if satisfies parents tag sels
                        then f t
                        else t
    in alterRec [] t

-- | Remove tags matching selector.
-- Does not remove the provided tag itself.
remove :: T.Text -> Tag -> Tag
remove sel t =
    let sels = parseSelector sel
        removeRec :: [Tag] -> Tag -> Tag
        removeRec parents tag = case tag of
            Tag n a c   -> Tag n a $ map (removeRec $ parents ++ [tag]) c
            otherwise   -> tag
    in removeRec [] t

-- | Returns tags matching the selector.
-- Obiously not too useful if you want to alter the given elements, because
-- of Haskell's purity. See alter and remove instead.
select :: T.Text -> Tag -> [Tag]
select sel t = 
    let sels = parseSelector sel
        selectRec :: [Tag] -> Tag -> [Tag]
        selectRec parents tag = case tag of
            Doctype t   -> retif tag
            Text t      -> retif tag
            Tag n a c   -> retif tag ++ (concat $ map (selectRec $ parents ++ [tag]) c)
            where
                retif t =
                    if satisfies parents tag sels
                        then [t]
                        else []
    in selectRec [] t

{--------------------------------------------------------------------
  Selectors.  
--------------------------------------------------------------------}

-- Selectors planned

-- Implemented      Example                     Description
--                  *                           - everything
-- Y                #X                          - id selector
-- Y                .X                          - class selector
-- Y                X Y                         - descendant selector
-- Y                X                           - type selector
-- Y                X > Y                       - direct child selector
-- Y                [attrName]                  - has attribute selector
-- Y                [attrName="val"]            - attribute name-value selector
-- Y                [attrName*="val"]           - regexp attribute selectors
-- Y                [attrName^="val"]  
-- Y                [attrName$="val"]  
--                  selector1, selector2        - multiple selectors
--                  selector:not(selector)      - negation pseudoclass selector
--                  selector:nth-child(3)   
--                  selector:nth-last-child(3)  

data Regexy =
        StartsWith
    |   EndsWith
    |   Contains
    |   Equals
    |   Any
    deriving (Eq, Show)

data Selector =
        Type        T.Text
    |   Id          T.Text
    |   Class       T.Text
    |   Attribute   Regexy T.Text T.Text     -- Regex type, tag name, attrname, attrval
    -- These are more like relations between selectors and not selectors themselves, but hey.
    |   Descendant
    |   DirectChild
    deriving (Eq, Show)

{--------------------------------------------------------------------
  Parsering selectors.  
--------------------------------------------------------------------}

parseString :: P.Parser T.Text
parseString = do
    P.char '"'
    x <- P.many (P.noneOf "\"")
    P.char '"'
    return $ T.pack x

symbol :: P.Parser Char
symbol = P.oneOf "-"

parseNonquoted :: P.Parser T.Text
parseNonquoted = do
    first <- P.letter P.<|> symbol
    rest <- P.many (P.letter P.<|> P.digit P.<|> symbol)
    return $ T.pack $ first:rest

parseDescendant :: P.Parser Selector
parseDescendant = do
    P.space
    return Descendant

parseDirectChild :: P.Parser Selector
parseDirectChild = do
    P.many P.space
    P.char '>'
    P.many P.space
    return DirectChild

parseId :: P.Parser Selector
parseId = do
    P.char '#'
    id <- parseNonquoted
    return $ Id id

parseClass :: P.Parser Selector
parseClass = do
    P.char '.'
    clas <- parseNonquoted
    return $ Class clas

parseTyp :: P.Parser Selector
parseTyp = do
    typ <- parseNonquoted
    return $ Type typ

parseAttr :: P.Parser Selector
parseAttr = do
    P.char '['
    attrName <- parseNonquoted
    mode <- P.many $ P.string "*=" P.<|> P.string "^=" P.<|> P.string "$=" P.<|> P.string "="
    val <- P.many $ parseNonquoted P.<|> parseString
    P.char ']'
    return $ case mode of
        []          -> Attribute Any        attrName    ""
        ["*="]      -> Attribute Contains   attrName    (val!!0)
        ["^="]      -> Attribute StartsWith attrName    (val!!0)
        ["$="]      -> Attribute EndsWith   attrName    (val!!0)
        ["="]       -> Attribute Equals     attrName    (f val)
    where
        f :: [T.Text] -> T.Text
        f x = if length x == 0
            then ""
            else x!!0

parseExpr :: P.Parser [Selector]
parseExpr = P.many1 $ P.try parseId
    P.<|> P.try parseClass
    P.<|> P.try parseAttr
    P.<|> P.try parseTyp
    P.<|> P.try parseDirectChild
    P.<|> P.try parseDescendant

-- > P.parse parseExpr "selector" "#id"