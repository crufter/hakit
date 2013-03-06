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
    matches, parseSelector, example
) where

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.List.Split as Spl

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
            text "Hello world.",
            div' [cat "class" "just-a-div"] [],
            div' [] [
                text "Hello again."
            ]
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
sc x = L.intercalate "" $ map (\y -> tabLines $ show y) x
    where tabLines x = unlines $ map (\y -> "    " ++ y) $ lines x

instance Show Tag where
    show (Doctype a)    = "<!DOCTYPE " ++ T.unpack a ++ ">"
    show (Tag n a c)    = case M.lookup n voidElements of
        Just ()     -> "<" ++ T.unpack n ++ sa a ++ "/>"
        Nothing     -> if length c > 0
            then "<" ++ T.unpack n ++ sa a ++ ">\n" ++ sc c ++ "</" ++ T.unpack n ++ ">"
            else "<" ++ T.unpack n ++ sa a ++ "></" ++ T.unpack n ++ ">"
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

-- Returns true of the tag or its descendants match the selector.
has :: Selector -> Tag -> Bool
has sel t = if matches' [] sel t
    then True
    else any (has sel) $ children t

calcRoot :: [Tag] -> Tag -> Tag
calcRoot parents tag = if length parents == 0
    then tag
    else parents!!0

filtIndex :: (Int -> Bool) -> [a] -> [a]
filtIndex pred xs =
    let indexes = [0 ..(length xs) - 1]
        iterable = zip indexes xs
    in map (\(_, b) -> b) $ filter (\(a, _) -> pred a) iterable

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
    Not selector    -> not $ matches' parents selector tag
    Has selector    -> any (has selector) $ children tag
    AncestorIs sel  -> if length parents == 0
        then False
        else let parinits = zip (L.inits parents) parents in
            any (\(pars, subj) -> matches' pars sel subj) parinits   
    ParentIs sel    -> if length parents == 0
        then False
        else matches' (init parents) sel $ last parents
    FirstChild      -> if length parents == 0
        then False
        else let ch = children $ last parents in if length ch == 0
            then False
            else head ch == tag
    LastChild       ->
        let pc = children $ last parents
        in if length parents == 0 || length pc == 1 -- A children can not be the first and last too at the same time.
            then False
            else last pc == tag
    Eq selector n   ->
        let root = calcRoot parents tag
            tm = take (n+1) $ select' selector root
        in if length tm <= n
            then False
            else (tm!!n) == tag
    -- Revisit this.
    LesserThan s n  ->
        let root = calcRoot parents tag
            tm = take n $ select' s root
        in any (== tag) tm
    GreaterThan s n ->
        let root = calcRoot parents tag
            tm = drop (n + 1) $ select' s root
        in any (== tag) tm
    First selector  ->
        let root = calcRoot parents tag
            tm = take 1 $ select' selector root
        in any (== tag) tm
    Last selector   ->
        let root = calcRoot parents tag
            tm = select' selector root
        in if length tm == 0
            then False
            else last tm == tag
    Even selector   ->
        let root = calcRoot parents tag
            tm = filtIndex (\x -> x `mod` 2 == 0) $ select' selector root
        in any (== tag) tm
    Odd selector    ->
        let root = calcRoot parents tag
            tm = filtIndex (\x -> x `mod` 2 == 1) $ select' selector root
        in any (== tag) tm
    NthChild n      ->
        let pc = children $ last parents
        in if length parents == 0 || length pc < n   -- Note that this selector is 1-indexed.
            then False
            else (pc!!(n - 1)) == tag
    NthLastChild n  ->
        let pc = children $ last parents
        in if length parents == 0 || length pc < n
            then False
            else (pc!!(length pc - n - 1)) == tag 
    Empty           -> length (children tag) == 0
    Parent          -> length (children tag) /= 0
    Any             -> True
    -- 
    Descendant      -> error $ "matches': bug: Descendant"
    DirectChild     -> error $ "matches': bug: DirectChild"
    Placeholder     -> error $ "matches': bug: Placeholder"
    Comma           -> error $ "matches': bug: Comma"

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
select sel t = let sels = parseSelector sel in select' sels t

select' :: Selector -> Tag -> [Tag]
select' sel t = 
    let selectRec :: [Tag] -> Tag -> [Tag]
        selectRec parents tag = case tag of
            Doctype t   -> retif tag
            Text t      -> retif tag
            Tag n a c   -> retif tag ++ (concat $ map (selectRec $ parents ++ [tag]) c)
            where
                retif t =
                    if matches' parents sel tag
                        then [t]
                        else []
    in selectRec [] t

{--------------------------------------------------------------------
  Selectors.  
--------------------------------------------------------------------}

-- Selectors planned.

-- Implemented      Example                     Name
-- Y                *                           - All selector
-- Y                #id                         - Id selector
-- Y                .class                      - Class selector
-- Y                selector1 selector2         - Descendant selector
-- Y                type                        - Type selector
-- Y                selector1 > selector2       - Direct child selector
-- Y                [attrName]                  - Has attribute selector
-- Y                [attrName="val"]            - Attribute equals selector
-- Y                [attrName*="val"]           - Attribute contains selector
-- Y                [attrName^="val"]           - Attribute starts with selector
-- Y                [attrName$="val"]           - Attribute ends with selector
-- Y                [attrName~="val"]           - Attribute contains word selector
-- Y                [attrName!="val"]           - Attribute not equals selector
-- Y                selector1, selector2        - Multiple selectors selector
-- Y                :not(selector)              - :not() selector
-- Y                :has(selector)              - :has() selector
-- Y                :eq(3)                      - :eq() selector
-- Y                :lt(3)                      - :lt() selector
-- Y                :gt(3)                      - :gt() selector
-- Y                :even                       - :even selector
-- Y                :odd                        - :odd selector
-- Y                :first                      - :first selector
-- Y                :last                       - :last selector
-- Y                :first-child                - :first-child selector
-- Y                :last-child                 - :last-child selector
-- Y                :nth-child(3)               - :nth-child() selector
-- Y                :nth-last-child(3)          - :nth-last-child() selector
-- Y                :empty                      - :empty selector
-- Y                :parent                     - :parent selector

data Regexy =
        StartsWith
    |   EndsWith
    |   Contains
    |   Equals
    |   Anything
    |   ContainsWord
    |   NotEquals
    deriving (Eq, Show)

data Selector =
        Any
    |   Type        T.Text
    |   Id          T.Text
    |   Class       T.Text
    |   Eq Selector Int | Even Selector | Odd Selector
    |   LesserThan Selector Int | GreaterThan Selector Int
    |   First Selector | Last Selector
    |   Parent | Empty
    |   FirstChild | LastChild | NthChild Int | NthLastChild Int
    -- Currently you can only apply flat selectors in an and.
    -- (eg: no descendant or direct child)
    |   And         [Selector]
    |   Or          [Selector]
    |   Not         Selector
    |   Has         Selector
    |   ParentIs    Selector
    |   AncestorIs  Selector
    |   Attribute   Regexy T.Text T.Text     -- Regex type, tag name, attrname, attrval
    -- Intermediate tokens
    |   Comma
    |   Descendant
    |   DirectChild
    |   Placeholder
    deriving (Eq, Show)

{--------------------------------------------------------------------
  Parsing selectors.  
--------------------------------------------------------------------}

parseSelector :: T.Text -> Selector
parseSelector t =
    let errMsg = "parseSelector: can't parse selector: " ++ show t
        sels = P.parse parseExpr errMsg $ T.unpack t
    in case sels of
        Left e      -> error $ show e
        Right ss    -> parseSelector' ss

parseSelector' :: [Selector] -> Selector
parseSelector' sels = orify sels
    where
    -- Transforms the flat structure selector1, or, selector2 to
    -- a nested or(selector1, selector2)
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
    -- Transforms the flat structure selector1 relation selector 2
    -- to a nested and(selector2, (relation(selector1)))
    parentify :: [Selector] -> Selector
    parentify selects =
        let checkValidity xs = if L.isInfixOf [Descendant, DirectChild] xs || L.isInfixOf [DirectChild, Descendant] xs
                then error $ "parseSelector': directchild and descendant tokens can't follow each other" ++ show xs
                else xs
            ws :: [[Selector]]
            ws = (Spl.split . Spl.oneOf) [Descendant, DirectChild] $ checkValidity selects
            fws = filter (\y -> length y > 0) ws
            -- [crit, sep, crit, sep, crit ... ]
            build xs =
                let (crit, sep) = L.partition (\z -> z /= Descendant && z /= DirectChild) $ reverse xs
                    buildRec a b = if length b == 0
                        then last a
                        else case head b of
                            Descendant      -> And [head a, AncestorIs $ buildRec (tail a) $ tail b]
                            DirectChild     -> And [head a, ParentIs $ buildRec (tail a) $ tail b]
                in buildRec crit sep
        in if length selects == 1
            then selects!!0
            else if length fws == 1
                then indexify $ head fws
                else build $ map (\x -> if length x == 1
                    then x!!0
                    else indexify x) fws
    -- Transforms
    -- sel1 ind -> ind(sel)
    -- sel1 ind sel2 -> and(ind(sel1), sel2)
    indexify :: [Selector] -> Selector
    indexify selects =
        let isInd x = case x of
                Eq _  _             -> True
                Even _              -> True
                Odd _               -> True
                First _             -> True
                Last _              -> True
                LesserThan _ _      -> True
                GreaterThan _ _     -> True
                otherwise           -> False
            setCrit x v = case x of
                Eq s  i             -> Eq v i
                LesserThan s i      -> LesserThan v i
                GreaterThan s i     -> GreaterThan v i
                Even s              -> Even v
                Odd s               -> Odd v
                First s             -> First v
                Last s              -> Last v
                otherwise           -> x
            ws :: [[Selector]]
            ws = (Spl.split . Spl.whenElt) isInd selects
            fws = filter (\y -> length y > 0) ws
            build xs = if not $ isInd $ last xs
                then And [last xs, buildRec $ reverse $ init xs]
                else buildRec $ reverse xs
                where
                buildRec xs = if length xs == 1
                    then if isInd (xs!!0)
                        then setCrit (xs!!0) Any
                        else xs!!0
                    else setCrit (xs!!0) $ buildRec $ tail xs
        in if length selects == 1
            then selects!!0
            else if length fws == 1
                then andify $ head fws
                else build $ map (\x -> if length x == 1
                    then x!!0
                    else andify x) fws
    andify :: [Selector] -> Selector
    andify selects = if length selects == 1
        then selects!!0
        else And selects

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

ts x = P.try $ P.string x

parseCons :: P.Parser Selector
parseCons = do
    c <- P.char '*' P.<|> P.char ':'
    case c of
        '*'     -> return Any
        ':'     -> do
            cons <- ts "empty" P.<|> P.string "parent"
                P.<|> ts "first-child" P.<|> ts "last-child"
                P.<|> P.string "first" P.<|> P.string "last"
                P.<|> P.string "even" P.<|> P.string "odd"
            return $ case cons of
                "parent"        -> Parent
                "empty"         -> Empty
                "last"          -> Last Placeholder
                "first"         -> First Placeholder
                "first-child"   -> FirstChild
                "last-child"    -> LastChild
                "even"          -> Even Placeholder
                "odd"           -> Odd Placeholder

parseNthChildEq :: P.Parser Selector
parseNthChildEq = do
    a <- ts ":nth-child(" P.<|> ts ":nth-last-child(" P.<|> ts ":eq("
        P.<|> ts ":lt(" P.<|> ts ":gt("
    num <- P.many1 P.digit
    P.char ')'
    return $ let n = (read num)::Int in case a of
        ":nth-child("       -> NthChild     n
        ":nth-last-child("  -> NthLastChild n
        ":eq("              -> Eq           Placeholder n
        ":lt("              -> LesserThan   Placeholder n
        ":gt("              -> GreaterThan  Placeholder n
        

parseNotHas :: P.Parser Selector
parseNotHas = do
    a <- ts ":not(" P.<|> ts ":has("
    sels <- parseExpr
    P.string ")"
    return $ case a of
        ":not("     -> Not $ parseSelector' sels
        ":has("     -> Has $ parseSelector' sels

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
    mode <- P.many $ P.string "*=" P.<|> P.string "^="
        P.<|> P.string "$=" P.<|> P.string "="
        P.<|> P.string "~=" P.<|> P.string "!="
    val <- P.many $ parseNonquoted P.<|> parseString
    P.char ']'
    return $ case mode of
        []          -> Attribute Anything       attrName    ""
        ["*="]      -> Attribute Contains       attrName    (val!!0)
        ["^="]      -> Attribute StartsWith     attrName    (val!!0)
        ["$="]      -> Attribute EndsWith       attrName    (val!!0)
        ["~="]      -> Attribute ContainsWord   attrName    (val!!0)
        ["!="]      -> Attribute NotEquals      attrName    (val!!0)
        ["="]       -> Attribute Equals         attrName    (f val)
    where
        f :: [T.Text] -> T.Text
        f x = if length x == 0
            then ""
            else x!!0

-- While try is not required everywhere,
-- they are there for simplicity and easier extendability.
parseExpr :: P.Parser [Selector]
parseExpr = P.many1 $ P.try parseId
    P.<|> P.try parseClass
    P.<|> P.try parseAttr
    P.<|> P.try parseTyp
    P.<|> P.try parseDirectChild
    P.<|> P.try parseDescendant
    P.<|> P.try parseCons
    P.<|> P.try parseComma
    P.<|> P.try parseNthChildEq
    P.<|> P.try parseNotHas

-- > P.parse parseExpr "selector" "#id"