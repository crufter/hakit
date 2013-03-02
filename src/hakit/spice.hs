{-# LANGUAGE OverloadedStrings #-}
module Hakit.Spice (
    -- * Tag creation
    tag, doctype, html, head', body, div', text, cat,
    -- * Nested tag functions
    alter, remove, select,
    -- * Single tag functions
    attrs, attr, children, name, addClass, removeClass, hasClass, toggleClass,
    -- * Types
    Attrs(), Tag(..), Child(..),
    -- * Exported for testing purposes only
    matches, parseSelector
) where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.List.Split as Spl
import Debug.Trace

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

{--------------------------------------------------------------------
  Rendering.  
--------------------------------------------------------------------}

voidElements :: M.Map T.Text ()
voidElements = M.fromList $ map (\x -> (x, ()))
    ["area", "base", "br", "col", "embed", "hr", "img", "input", "keygen", "link", "menuitem", "meta", "param", "source", "track", "wbr"]

instance Show Attrs where
    show (Attrs at) = L.intercalate " " $ map (\(a, b) -> T.unpack a ++ "=" ++ show b) $ M.toList at

-- Show attributes
sa :: Attrs -> String
sa a@(Attrs at) = if M.size at > 0
        then " " ++ show a
        else ""

-- Show children.
sc :: [Child] -> String
sc x = L.intercalate "\n" $ map show x

instance Show Tag where
    show (Doctype a)    = "<!DOCTYPE " ++ T.unpack a ++ ">"
    show (Tag n a b)    = case M.lookup n voidElements of
        Just ()     -> "<" ++ T.unpack n ++ sa a ++ "/>"
        Nothing     -> "<" ++ T.unpack n ++ sa a ++ ">" ++ sc b ++ "</" ++ T.unpack n ++ ">"
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
matches :: T.Text -> Tag -> Bool
matches sel tag = matches' [] (parseSelector sel) tag

-- Returns true if given tag matches the selector provided.
matches' :: [Tag] -> Selector -> Tag -> Bool
matches' parents s tag = case s of
    Type t  -> name tag == t
    Id  id  -> case attr "id" tag of
        Just x  -> x == id
        Nothing -> False
    Class c -> hasClass c tag
    Attribute reg attrName attrVal -> case attr attrName tag of
        Nothing -> False
        Just x  -> case reg of
            StartsWith  -> T.isPrefixOf attrVal x
            EndsWith    -> T.isSuffixOf attrVal x
            Contains    -> T.isInfixOf attrVal x
            Equals      -> x == attrVal
            Anything    -> True
    And selectors   -> all (\x -> matches' parents x tag) selectors
    Or selectors    -> any (\x -> matches' parents x tag) selectors
    AncestorIs sel  -> if length parents == 0
        then False
        else if matches' (init parents) sel $ last parents
            then True
            else matches' ((init . init) parents) sel $ (last . init) parents
    ParentIs sel    -> if length parents == 0
        then False
        else matches' (init parents) sel $ last parents
    Empty           -> length (children tag) == 0
    Parent          -> length (children tag) /= 0
    Any             -> True
    -- 
    Descendant      -> error $ "matches: bug: Descendant"
    DirectChild     -> error $ "matches: bug: should never get to DirectChild"

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
                    if matches' parents sels tag
                        then f t
                        else t
    in alterRec [] t

-- | Remove tags matching the selector.
-- Does not remove the provided tag itself.
remove :: T.Text -> Tag -> Tag
remove sel t =
    let sels = parseSelector sel
        removeRec :: [Tag] -> Tag -> Tag
        removeRec parents tag = case tag of
            Tag n a c   -> Tag n a $ filter (matches' parents sels) $ map (removeRec $ parents ++ [tag]) c
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
                    if matches' parents sels tag
                        then [t]
                        else []
    in selectRec [] t

{--------------------------------------------------------------------
  Selectors.  
--------------------------------------------------------------------}

-- Selectors planned

-- Implemented      Example                     Description
-- Y                *                           - matches any element
-- Y                #X                          - id selector
-- Y                .X                          - class selector
-- Y                selector1 selector2         - descendant selector
-- Y                X                           - type selector
-- Y                selector1 > selector2       - direct child selector
-- Y                [attrName]                  - has attribute selector [name]
-- Y                [attrName="val"]            - attribute name-value selector
-- Y                [attrName*="val"]           - regexp attribute selectors
-- Y                [attrName^="val"]  
-- Y                [attrName$="val"]  
-- Y                selector1, selector2        - multiple selectors
--                  :not(selector)              - :not() selector
--                  :eq(3)
--                  :first
--                  :last
--                  :first-child
--                  :last-child
--                  :nth-child(3)   
--                  :nth-last-child(3)
-- Y                :empty
-- Y                :parent

data Regexy =
        StartsWith
    |   EndsWith
    |   Contains
    |   Equals
    |   Anything
    deriving (Eq, Show)

data Selector =
        Type        T.Text
    |   Id          T.Text
    |   Class       T.Text
    |   Eq          Int
    |   Any | First | Last | Parent | Empty
    |   FirstChild | LastChild | NthChild Int | NthLastChild Int
    -- Currently you can only apply flat selectors in an and.
    -- (eg: no descendant or direct child)
    |   And         [Selector]
    |   Or          [Selector]
    |   ParentIs    Selector
    |   AncestorIs  Selector
    |   Attribute   Regexy T.Text T.Text     -- Regex type, tag name, attrname, attrval
    -- Intermediate tokens
    |   Comma
    |   Descendant
    |   DirectChild
    deriving (Eq, Show)

{--------------------------------------------------------------------
  Parsing selectors.  
--------------------------------------------------------------------}

parseSelector :: T.Text -> Selector
parseSelector t =
    let sels = P.parse parseExpr "selector" $ T.unpack t
        orify :: [Selector] -> Selector
        orify selects =
            let ws :: [[Selector]]
                ws = (Spl.split . Spl.dropDelims . Spl.oneOf) [Comma] selects
                fws = filter (\y -> length y > 0) ws
            in if length selects == 1
                then selects!!0
                else if length fws == 1
                    then parentify $ head fws
                    else Or $ map (\x -> if length x == 1
                        then x!!0
                        else parentify x) fws
        parentify :: [Selector] -> Selector
        parentify selects =
            let checkValidity xs = if L.isInfixOf [Descendant, DirectChild] xs || L.isInfixOf [DirectChild, Descendant] xs
                    then error $ "parseSelector: directchild or descendant tokens can't follow each other" ++ show xs
                    else xs
                ws :: [[Selector]]
                ws = (Spl.split . Spl.oneOf) [Descendant, DirectChild] $ checkValidity selects
                fws = filter (\y -> length y > 0) ws
                -- [crit, sep, crit, sep, crit ... ]
                build xs =
                    let (crit, sep) = L.partition (\z -> z /= Descendant && z /= DirectChild) xs
                        buildRec a b = if length b == 0
                            then last a
                            else case head b of
                                Descendant      -> And [head a, AncestorIs $ buildRec (tail a) $ tail b]
                                DirectChild     -> And [head a, ParentIs $ buildRec (tail a) $ tail b]
                    in buildRec crit sep
            in if length selects == 1
                then selects!!0
                else if length fws == 1
                    then And $ head fws
                    else build $ reverse $ map (\x -> if length x == 1
                        then x!!0
                        else And x) fws
    in case sels of
        Left e      -> error $ show e
        Right ss    -> orify ss

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

parseCons :: P.Parser Selector
parseCons = do
    cons <- P.string ":parent" P.<|> P.string ":empty" P.<|> P.string ":last" P.<|> P.string ":first"
            P.<|> P.string ":first-child" P.<|> P.string ":last-child"
    return $ case cons of
        ":parent"       -> Parent
        ":empty"        -> Empty
        ":last"         -> Last
        ":first"        -> First
        "*"             -> Any
        ":first-child"  -> FirstChild
        ":last-child"   -> LastChild

-- parseNthChild :: P.Parser Selector
-- parseNthChild = do
--     P.string ":parent"

parseComma :: P.Parser Selector
parseComma = do
    P.many P.space
    P.char ','
    P.many P.space
    return Comma

parseAttr :: P.Parser Selector
parseAttr = do
    P.char '['
    attrName <- parseNonquoted
    mode <- P.many $ P.string "*=" P.<|> P.string "^=" P.<|> P.string "$=" P.<|> P.string "="
    val <- P.many $ parseNonquoted P.<|> parseString
    P.char ']'
    return $ case mode of
        []          -> Attribute Anything   attrName    ""
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
    P.<|> P.try parseCons
    P.<|> P.try parseComma

-- > P.parse parseExpr "selector" "#id"