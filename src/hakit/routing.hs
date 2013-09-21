{-# LANGUAGE OverloadedStrings #-}

module Hakit.Routing (

) where

import Hakit (DocLike, showWithoutQuotes, dm)
import qualified Data.Text as T
import qualified Data.Map as M

--routeValueMarker :: T.Text
--routeValueMarker = ":"
--
---- | A route is essentially a link with placeholder values.
---- "/cars/:id"
--data Route
--    = RouteString   T.Text
--    | RouteNum      Integer 
--
--class RouteLike a where
--    toRoute     :: a -> Route
--    fromRoute   :: Route -> a

--parseRoute :: T.Text -> Route
--parseRoute t =
--    let pairs = zip scheme $ splitNorm t
--        tester (a,b) = if T.head a == routeValueMarker
--            then Just (T.tail a, b)
--            else if a == b
--                then Just (a, b)
--                else Nothing
--        mPairs = sequence $ map tester pairs
--    in case mPairs of
--        Just mpr    -> Just . Location scheme . interpretDoc $ map (\(a, b) -> (TE.encodeUtf8 a, Just (TE.encodeUtf8 b))) mpr
--        Nothing     -> Nothing

--instance RouteLike T.Text where
--    toRoute = 
--
--instance String T.Text where
--    parseRoute = parseRoute . T.pack

-- | Serializes a route, using an input document as sources
-- for placeholders, eg:
-- serialize "/cars/:id" ["id" .- "3905"] == "/cats/3095"
--serialize :: (DocLike d, RouteLike r) => r -> d -> T.Text
--serialize  r d' =
--    let d = dm d'
--        a = splitPath r
--    in "/" ++ case a of
--        []          -> ""
--        otherwise   ->
--            let f x = case T.length x of
--                    0           -> ""
--                    1           -> x
--                    otherwise   -> if T.head x == routeValueMarker
--                        then case M.lookup (T.tail x) d of
--                            Just docv   -> T.pack $ showWithoutQuotes docv
--                            Nothing     -> error $ "Can't find element: " ++ show x
--                        else x
--            in T.intercalate "/" $ map f a

-- | Splits a T.Text by forward dashes, and filters the empty ones.
-- "/cars/114/comments/9"   -> ["cars", "114", "comments", 9]
--splitPath :: T.Text -> [T.Text]
--splitPath x = filter (\x -> T.length x > 0) $ T.splitOn "/" x

-- -- | Returns true if an url matches a route.
-- matchesStructure :: Route -> [T.Text] -> Bool
-- matches r p = 

-- | Same as matches but
-- matchesValues :: DocComp d => Route -> [T.Text] -> d -> Bool