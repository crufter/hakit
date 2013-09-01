{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ExtendedDefaultRules   #-}

module Tests.Hakit.TestHakit where

import Test.HUnit
import Hakit
import qualified Data.Text as T

want what got verdict = assertBool (show what ++ "\n" ++ show got) verdict

cases1 = [
    (["author" .- ["name" .- "Joe", "age" .- 30]],                                  "author.name", d "Joe"),
    (["x" .- ["y" .- ["z" .- "val"]]],                                              "x.y.z", d "val"),
    (["x" .- [d "a", d "b", d "c"]],                                                "x[0]", d "a"),
    (["x" .- ["y" .- [d "a", d ["field1" .- "val1", "field2" .- "val2"], d "c"]]],  "x.y[1].field2", d "val2")
    ]
t f cs = TestCase $ mapM_ f1 cs
    where
    f1 c =
        let res = f (e2 c) (dm $ e1 c)
            check = e3 c
        in want check res $ res == check
    
testGet = t get cases1

cases2 = [
    (["x" .- ["y" .- "Yo."]],   "x.y", True),
    (["x" .- ["y" .- "Hey."]],  "x.z", False)
    ]
testExists = t exists cases2

cases3 = [
    (["x" .- "Boo"], ("x", "Wut?"), ["x" .- "Wut?"]),                   -- Modifying existing key.
    (["x" .- "Boo"], ("y", "Wut?"), ["y" .- "Wut?", "x" .- "Boo"]),     -- Setting nonexisting key.
        (
        ["x" .- "y"],
        ("a.b.c.d", "X"),
        [
            "x" .- "y",
            "a" .- [
                "b" .- [
                    "c" .- [
                        "d" .- "X"
                    ]
                ]
            ]
        ]
    ),
    (
        ["x" .- "y"],
        ("x.a", "X"),
        ["x" .- "y"]
    ),
    (
        [
            "x" .- [
                "y" .- "Hey."
            ]
        ],
        ("x.z", "Hi."),
        [
            "x" .- [
                "y" .- "Hey.",
                "z" .- "Hi."
            ]
        ]
    ),
    (
        [
            "x" .- [
                "y" .- "Cheers.",
                "z" .- [d 42, d ["a" .- ["b" .- ["c" .- "X"]]]]
            ]
        ],
        ("x.z.1.a.b.d", "Y"),
        [
            "x" .- [
                "y" .- "Cheers.",
                "z" .- [d 42, d ["a" .- ["b" .- ["c" .- "X", "d" .- "Y"]]]]
            ]
        ]
    ),
    (
        [
            "x" .- [
                "y" .- "Cheers.",
                "z" .- [d 42, d "X"]
            ]
        ],
        ("x.z.1", "Y"),
        [
            "x" .- [
                "y" .- "Cheers.",
                "z" .- [d 42, d "Y"]
            ]
        ]
    ),
    (
        [
            "x" .- [
                "y" .- [d 42]
            ]
        ],
        ("x.y.1", "Hey."),
        [
            "x" .- [
                "y" .- [d 42]
            ]
        ]
    )
    ]
testSet = TestCase $ mapM_ f cases3
    where
    f c =
        let res = set (fst $ e2 c) (snd $ e2 c) (dm $ e1 c)
            check = dm $ e3 c
        in want check res $ res == check

cases4 :: [([(T.Text, DocVal)], T.Text, [(T.Text, DocVal)])]
cases4 = [
    (
        ["x" .- "Boo"],
        "x",
        []
    ),
    (
        [
            "x" .- [
                "y" .- "Cheers.",
                "z" .- [d 42, d ["a" .- ["b" .- ["c" .- "X"]]]]
            ]
        ],
        "x.z",
        [
            "x" .- [
                "y" .- "Cheers."
            ]
        ]
    ),
    (
        [
            "x" .- [
                "y" .- "Cheers.",
                "z" .- [d 42, d ["a" .- ["b" .- ["c" .- "X"]], "d" .- "Y"]]
            ]
        ],
        "x.z.1.a",
        [
            "x" .- [
                "y" .- "Cheers.",
                "z" .- [d 42, d ["d" .- "Y"]]
            ]
        ]
    )
    ]
testUnset = TestCase $ mapM_ f cases4
    where
    f c =
        let res = unset (e2 c) (dm $ e1 c)
            check = dm $ e3 c
        in want check res $ res == check

cases5 = [
    ("{\"a\":42}", ["a" .- 42])
    ]
testFromJSON = TestCase $ mapM_ f cases5
    where
    f c =
        let parsed = fromJSON $ fst c
            check = dm $ snd c
        in want check parsed $ parsed == check

cases6 = [
    (["a" .- 42], "{\"a\":42}")
    ]
testToJSON = TestCase $ mapM_ f cases6
    where
    f c =
        let stringed = toJSON . dm $ fst c
            check = snd c
        in want check stringed $ stringed == check

tests = TestList [
    TestLabel "testGet" testGet,
    TestLabel "testExists" testExists,
    TestLabel "testSet" testSet,
    TestLabel "testUnset" testUnset,
    TestLabel "testFromJSON" testFromJSON,
    TestLabel "testToJSON" testToJSON
    ]
