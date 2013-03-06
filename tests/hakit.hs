{-# LANGUAGE OverloadedStrings #-}
module Tests.Hakit where

import Test.HUnit
import Hakit

e1 (x,_,_) = x
e2 (_,x,_) = x
e3 (_,_,x) = x

want what verdict = assertBool (show what) verdict

cases1 = [
    (["author" .- ["name" .- "Joe", "age" .- (30]],                                 "author.name", d "Joe"),
    (["x" .- ["y" .- ["z" .- "val"]]],                                              "x.y.z", d "val"),
    (["x" .- [d "a", d "b", d "c"]],                                                "x[0]", d "a"),
    (["x" .- ["y" .- [d "a", d ["field1" .- "val1", "field2" .- "val2"], d "c"]]],  "x.y[1].field2", d "val2")
    ]
t f c = TestCase $ mapM_ (\c -> want c ((f (e2 c) (e1 c)) == e3 c)) c
testGet = t get cases1

cases2 = [
    (["x" .- ["y" .- "Yo."]],   "x.y", True),
    (["x" .- ["y" .- "Hey."]],  "x.z", False)
    ]
testExists = t exists cases2

cases3 = [
    (["x" .- "Boo"], ("x", "Wut?"), ["x" .- "Wut?"]),                   -- Modifying existing key.
    (["x" .- "Boo"], ("y", "Wut?"), ["y" .- "Wut?", "x" .- "Boo"])      -- Setting nonexisting key.
    ]
testSet = TestCase $ mapM_ (\c -> want c (set ((fst . e2) c) (e1 c) ((snd . e2) c) == e3 c)) cases3

tests = TestList [
    TestLabel "testGet" testGet,
    TestLabel "testExists" testExists,
    TestLabel "testSet" testSet
    ]

