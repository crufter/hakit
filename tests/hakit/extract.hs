{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Tests.Hakit.Extract where

import Test.HUnit
import Hakit
import Hakit.Extract

isLeft :: Either a b -> Bool
isLeft a = case a of
    Left a -> True
    Right a -> False

isRight :: Either a b -> Bool
isRight a = not $ isLeft a

want what verdict = assertBool (show what) verdict

i x = x::Integer

cases1 = [
    (["a" .- "Exampl"],                     ["a" .- ["min" .- i 7]],                isLeft),        -- String min
    (["a" .- "Example sentence."],          ["a" .- ["max" .- i 5]],                isLeft),        -- String max
    (["a" .- "Exampl"],                     ["a" .- ["min" .- i 6, "max" .- i 6]],  isRight),       -- String min, max
    (["a" .- i 40], ["a" .- ["type" .- "int", "min" .- i 50]],                      isLeft),        -- Int min
    (["a" .- i 30], ["a" .- ["type" .- "int", "max" .- i 20]],                      isLeft),        -- Int max
    (["a" .- i 20], ["a" .- ["type" .- "int", "min" .- i 20, "max" .- i 20]],       isRight),       -- Int min, max
    ([], ["a" .- ["type" .- "const", "value" .- True]], (\x -> x == Right (dm ["a" .- True]))),     -- Const
    (["a" .- [d "a", d "b", d "c"]], ["a" .- ["isList" .- True]], (\x -> x == Right (dm ["a" .- [d "a", d "b", d "c"]]))) -- Simple list
    ]
t c = TestCase $ mapM_ (\c -> want (extractSafe (dm $ e1 c) (dm $ e2 c)) (e3 c (extractSafe (dm $ e1 c) (dm $ e2 c)) == True)) c
testGet = t cases1

tests = TestList [
    TestLabel "testGet" testGet
    ]

