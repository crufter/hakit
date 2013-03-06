{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Tests.Hakit.Validation where

import Test.HUnit
import Hakit
import Hakit.Validation

want what verdict = assertBool (show what) verdict

cases1 = [
    (["a" .- "Exampl"],                     ["a" .- ["min" .- 7]],                  isLeft),        -- String min
    (["a" .- "Example sentence."],          ["a" .- ["max" .- 5]],                  isLeft),        -- String max
    (["a" .- "Exampl"],                     ["a" .- ["min" .- 6, "max" .- 6]],      isRight),       -- String min, max
    (["a" .- 40], ["a" .- ["type" .- "int", "min" .- 50]],                          isLeft),        -- Int min
    (["a" .- 30], ["a" .- ["type" .- "int", "max" .- 20]],                          isLeft),        -- Int max
    (["a" .- 20], ["a" .- ["type" .- "int", "min" .- 20, "max" .- 20]],             isRight),       -- Int min, max
    (["a" .- 99], ["a" .- ["type" .- "bool"]],                                      isLeft),        -- Type mismatch
    (["b" .- 99], ["b" .- ["type" .- "bool", "ignore" .- True]],                    isRight),       -- Ignore, with it, the type mismatch disappears
    ([], ["a" .- ["type" .- "const", "value" .- True]], (\x -> x == Right (dm ["a" .- True]))),     -- Const
    (   ["a" .- [d "a", d "b", d "c"]],
        ["a" .- ["isList" .- True]],
        (\x -> x == Right (dm ["a" .- [d "a", d "b", d "c"]]))
    ), -- Simple list
    (["a" .- [d "a"]],                      ["a" .- ["isList" .- True, "listMin" .- 2]], isLeft),     -- Too short list
    (["a" .- [d "a", d "b", d "c"]],        ["a" .- ["isList" .- True, "listMax" .- 2]], isLeft),     -- Too long list
    (
        ["a" .- [d False, d True, d "a"]],
        ["a" .- ["isList" .- True, "type" .- "bool"]],
        isLeft
    ), -- List member type mismatch.
    (
        ["a" .- [d False, d True, d "a"]],
        ["a" .- ["isList" .- True, "type" .- "bool", "listIgnore" .- True]],
        isRight
    ) -- List ignore, with it, the list member type mismatch disappears.
    ]

t c = TestCase $ mapM_ (\c -> want (validateSafe (dm $ e2 c) (dm $ e1 c)) (e3 c (validateSafe (dm $ e2 c) (dm $ e1 c)) == True)) c
testGet = t cases1

tests = TestList [
    TestLabel "testGet" testGet
    ]
