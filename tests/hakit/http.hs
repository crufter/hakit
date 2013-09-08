{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}

module Tests.Hakit.Http where

import Test.HUnit
import Hakit
import qualified Hakit.Http as Ht
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M

want what verdict = assertBool (show what) verdict

cases1 = [
    ["id" .- "122"]
    ]

cookieHs :: Document -> [T.Text]
cookieHs doc =
    let f (k, v) = T.concat [
            "Set-Cookie: ", k, "=", v, "; Max-Age=3600; Version=1"
            ]
        f1 (k, v) = (k, toString v)
    in L.sort . map (f . f1) $ M.toList doc

t cs = TestCase $ mapM_ f cs
    where
    f c' =
        let c = dm c
            getCook     = Ht.cookies $ Ht.setCookies c Ht.resp
        in want getCook $ getCook == c
testCookies = t cases1

tests = TestList [
    TestLabel "testCookies" testCookies
    ]
