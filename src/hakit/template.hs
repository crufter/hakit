{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.List as L
import qualified Text.ParserCombinators.Parsec as P

-- This package contains some questionable temporary names now to avoid clash with prelude.

data Attr = Attr (T.Text, T.Text)

instance Show Attr where
    show (Attr (a, b)) = T.unpack a ++ "=" ++ show b

type Child = Tag

data Tag =
        Doctype 
    |   HTML    [Attr] [Child]
    |   Head    [Attr] [Child]
    |   Body    [Attr] [Child]
    |   Div     [Attr] [Child]
    |   Text    T.Text
    |   A       [Attr] [Child]

html a c    = HTML a c
head' a c   = Head a c
body a c    = Body a c
text t      = Text t
div' a c    = Div a c

attr :: T.Text -> T.Text -> Attr
attr a b = Attr (a, b)

example :: Tag
example =
    HTML [] [
        Head [] [],
        Body [attr "style" "background: #ccc;"] [
            Text "yo"
        ]
    ]

example2 :: Tag
example2 =
    html [] [
        head' [] [],
        body [attr "style" "background: #ccc;"] [
            text "yo"
        ]
    ]

-- Show attributes
sa :: [Attr] -> String
sa x = let attrs = L.intercalate " " $ map show x in
    if length attrs > 0
        then " " ++ attrs
        else ""

-- Show children.
sc :: [Child] -> String
sc x = L.intercalate "\n" $ map show x

instance Show Tag where
    show (HTML a b) = "<html" ++ sa a ++ ">" ++ sc b ++ "</html>"
    show (Head a b) = "<head" ++ sa a ++ ">" ++ sc b ++ "</head>"
    show (Body a b) = "<body" ++ sa a ++ ">" ++ sc b ++ "</body>"
    show (Text a)   = T.unpack a

-- Planned supported selectors.
-- *                    - everything
-- #X                   - id selector
-- .X                   - class selector
-- X Y                  - descendant selector
-- X                    - type selector
-- X > Y                - direct child selector
-- X[attrName]          - has attribute selector
-- X[attrName="val"]    - attribute name-value selector
-- X[attrName*="val"]   - regexp attribute selectors
-- X[attrName^="val"]
-- X[attrName$="val"]
-- :not(selector)       - negation pseudoclass selector
-- :nth-child(3)
-- :nth-last-child(3)

-- map, just named differently, to avoid clash with Prelude.
-- change

data Regexy =
        StartsWith
    |   EndsWith
    |   Contains
    |   Equals
    |   Any
    deriving (Show)

data Selector =
        Type        T.Text
    |   Id          T.Text
    |   Class       T.Text
    |   Attribute   Regexy T.Text T.Text T.Text     -- Regex type, tag name, attrname, attrval
    -- These are more like relations between selectors and not selectors themselves.s
    |   Descendant
    |   DirectChild
    deriving (Show)

{--------------------------------------------------------------------
  Parsers.  
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
    P.many P.space
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
    typ <- parseNonquoted
    P.many P.space
    P.char '['
    attrName <- parseNonquoted
    mode <- P.many $ P.string "*=" P.<|> P.string "^=" P.<|> P.string "$=" P.<|> P.string "="
    val <- P.many $ parseNonquoted P.<|> parseString
    P.char ']'
    return $ case mode of
        []          -> Attribute Any        attrName    typ     ""
        ["*="]      -> Attribute Contains   attrName    typ     (val!!0)
        ["^="]      -> Attribute StartsWith attrName    typ     (val!!0)
        ["$="]      -> Attribute EndsWith   attrName    typ     (val!!0)
        ["="]       -> Attribute Equals     attrName    typ     (f val)
    where
        f :: [T.Text] -> T.Text
        f x = if length x == 0
            then ""
            else x!!0

parseExpr :: P.Parser Selector
parseExpr = P.try parseId
    P.<|> P.try parseClass
    P.<|> P.try parseAttr
    P.<|> P.try parseTyp
    P.<|> P.try parseDirectChild
    P.<|> P.try parseDescendant

-- parse parseExpr "selector" input