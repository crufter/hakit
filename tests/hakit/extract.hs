{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Tests.Hakit.Extract where

import Test.HUnit
import Hakit
import Hakit.Extract

want what verdict = assertBool (show what) verdict

i x = x::Integer

cases1 = [
    (["a" .- "Exampl"],                     ["a" .- ["min" .- i 7]],                isLeft),        -- String min
    (["a" .- "Example sentence."],          ["a" .- ["max" .- i 5]],                isLeft),        -- String max
    (["a" .- "Exampl"],                     ["a" .- ["min" .- i 6, "max" .- i 6]],  isRight),       -- String min, max
    (["a" .- i 40], ["a" .- ["type" .- "int", "min" .- i 50]],                      isLeft),        -- Int min
    (["a" .- i 30], ["a" .- ["type" .- "int", "max" .- i 20]],                      isLeft),        -- Int max
    (["a" .- i 20], ["a" .- ["type" .- "int", "min" .- i 20, "max" .- i 20]],       isRight),       -- Int min, max
    (["a" .- i 99], ["a" .- ["type" .- "bool"]],                                    isLeft),        -- Type mismatch
    (["b" .- i 99], ["b" .- ["type" .- "bool", "ignore" .- True]],                  isRight),       -- Ignore, with it, the type mismatch disappears
    ([], ["a" .- ["type" .- "const", "value" .- True]], (\x -> x == Right (dm ["a" .- True]))),     -- Const
    (   ["a" .- [d "a", d "b", d "c"]],
        ["a" .- ["isList" .- True]],
        (\x -> x == Right (dm ["a" .- [d "a", d "b", d "c"]]))
    ), -- Simple list
    (["a" .- [d "a"]],                      ["a" .- ["isList" .- True, "listMin" .- i 2]], isLeft),     -- Too short list
    (["a" .- [d "a", d "b", d "c"]],        ["a" .- ["isList" .- True, "listMax" .- i 2]], isLeft),     -- Too long list
    (   ["a" .- [d False, d True, d "a"]],
        ["a" .- ["isList" .- True, "type" .- "bool"]],
        isLeft
    ), -- List member type mismatch.
    (
        ["a" .- [d False, d True, d "a"]],
        ["a" .- ["isList" .- True, "type" .- "bool", "listIgnore" .- True]],
        isRight
    ) -- List ignore, with it, the list member type mismatch disappears.
    ]

t c = TestCase $ mapM_ (\c -> want (extractSafe (dm $ e1 c) (dm $ e2 c)) (e3 c (extractSafe (dm $ e1 c) (dm $ e2 c)) == True)) c
testGet = t cases1

tests = TestList [
    TestLabel "testGet" testGet
    ]
