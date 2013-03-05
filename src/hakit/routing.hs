{-# LANGUAGE OverloadedStrings #-}

module Hakit.Routing (
    parseLocation
) where

import Hakit (Location(..))
import Hakit.Server(queryToDoc) -- Ehh, looks ugly.
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

-- | Splits a T.Text by forward dashes, and filters the empty ones.
-- "/cars/114/comments/9"   -> ["cars", "114", "comments", 9]
splitNorm :: T.Text -> [T.Text]
splitNorm x = filter (\x -> T.length x > 0) $ T.splitOn "/" x

parseLocation :: [T.Text] -> T.Text -> Maybe Location
parseLocation scheme t =
    let pairs = zip scheme $ splitNorm t
        tester (a,b) = if T.head a == '#'
            then Just (T.tail a, b)
            else if a == b
                then Just (a, b)
                else Nothing
        mPairs = sequence $ map tester pairs
    in case mPairs of
        Just mpr    -> Just $ Location scheme $ queryToDoc $ map (\(a, b) -> (TE.encodeUtf8 a, Just (TE.encodeUtf8 b))) mpr
        Nothing     -> Nothing