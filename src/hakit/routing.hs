{-# LANGUAGE OverloadedStrings #-}

module Hakit.Routing (
    Route,
    serialize,
    splitPath,
    matches,
    matchesText
) where

import Hakit (raw)
import qualified Data.Text as T

routeValueMarker :: T.Text
routeValueMarker = ":"

-- | A route is essentially a link with placeholder values.
-- "/cars/:id"
data Route
    = RouteString   T.Text
    | RouteNum      Integer 

class RouteLike a where
    toRoute     :: a -> Route
    fromRoute   :: Route -> a

parseRoute :: T.Text -> Route
parseRoute :: T.

instance RouteLike T.Text where
    parseRoute t = parseRoute

instance String T.Text where
    parseRoute = parseRoute . T.pack

-- | Serializes a route, using an input document as sources
-- for placeholders, eg:
-- serialize "/cars/:id" ["id" .- "3905"] == "/cats/3095"
serialize :: (DocComp d, RouteLike r) => r -> d -> T.Text
serialize  r d' =
    let d = dm d'
        a = splitPath r
    in "/" ++ case a of
        []          -> ""
        otherwise   ->
            let f x = case T.length x of _
                    0           -> ""
                    1           -> x
                    otherwise   -> T.head x == routeValueMarker
                        then case M.lookup (T.tail x) b of
                            Just docv   -> raw docv
                            Nothing     -> error $ "Can't find element: " ++ show x
                        else x
            in T.intercalate "/" $ map f a

-- | Splits a T.Text by forward dashes, and filters the empty ones.
-- "/cars/114/comments/9"   -> ["cars", "114", "comments", 9]
splitPath :: T.Text -> [T.Text]
splitPath x = filter (\x -> T.length x > 0) $ T.splitOn "/" x

-- | Returns true if an url matches a route.
matchesStructure :: Route -> [T.Text] -> Bool
matches r p = 

-- | Same as matches but
matchesValues :: DocComp d => Route -> [T.Text] -> d -> Bool