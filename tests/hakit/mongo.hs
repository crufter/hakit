{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Tests.Hakit.Mongo where

import Test.HUnit
import qualified Hakit.Mongo as M
import Hakit
import qualified Data.Text as T
import qualified Safe
import Debug.Trace

-- This test suite is database agnostic, so we should separate it if we start to support dbs other than mongo.
-- Assuming a running mongoDB instance on 127.0.0.1:27017.
-- Note: the connections are not getting closed (in ghci at least).

dbConn = connect M.new ["server" .- "127.0.0.1", "serverName" .- "prim", "db" .- "testDb"]

want wanted got verdict = assertBool ("wanted: " ++ show wanted ++ "\ngot:    " ++ show got) verdict

testDropDb = TestCase $ f >>= \(a, b) -> assertBool a b where
    loc = ["db" .- "dropTestDb", "coll" .- "testColl"]
    f = do
        db <- dbConn
        let d = setLocation db loc
            doc = toDoc ["a" .- "b"]
        insert d doc
        docs <- find d nilDoc
        if length docs == 0 || unset "id" (docs!!0) /= doc
            then return ("Can't insert", False)
            else do
                dropDb db loc
                docs <- find d nilDoc
                if length docs /= 0
                    then return ("Can't drop db.", False)
                    else return ("", True)

cases1 = [
    ["name" .- "Joey", "age" .- (33::Integer), "favRat" .- (3.14::Double), "isOk" .- True],     -- Basic data types.
    [   "cat" .- dbRef ["coll" .- "categories", "id" .- "11234"],
        "tags" .- [dbRef ["coll" .- "tags", "id" .- "1212121"], dbRef ["coll" .- "tags","id" .- "723312"]]
    ],   -- dbRef single and dbRef list.
    ["mixed" .- [dbRef ["coll" .- "whatever", "id" .- "534232"], d "Yo", d (12::Integer)]],   -- dbRef list mixed with other datatypes.
    ["id" .- oid "510bc373b65bd004a8000008", "hey" .- "ho"],    -- Simple objectId test.
    ["id" .- oidPad "0", "a" .- "b"]    -- Strange ObjectIds should work as long as they have proper length.
    ]

testDI = TestCase $ mapM_ (\c -> do {(got, verdict) <- f c; want c got verdict}) cases1 where
    f doc1 = do
        let doc = toDoc doc1
        db <- dbConn
        dropCurrent db
        let dbc = setLocation db ["coll" .- "testCollection"]
        insert dbc doc
        docs <- find dbc nilDoc
        let docF = Safe.atNote "testDI: no doc found" docs 0
        let got = if exists "id" doc then docF else unset "id" docF
        if got == doc then return (got, True) else return (got, False)

pad s = let str = T.unpack s in T.pack $ replicate (24 - length str) (Safe.atNote "Str len is 0" (show 0) 0) ++ str
oidPad str = oid $ pad str

-- [([list of (collname, doc) to insert], collNameToGetFirstDocFrom, resolvedDocMustLookLikeThis)]
cases2 = [
        (
            [
                ("coll1", ["id" .- oidPad "1", "field" .- "val", "field1" .- dbRef ["coll" .- "coll2", "id" .- pad "0"]]),
                ("coll2", ["id" .- oidPad "0", "hey" .- "ho"])
            ],
            "coll1",
            ["id" .- oidPad "1", "field" .- "val", "field1" .- ["id" .- oidPad "0", "hey" .- "ho"]]
        ),
        (
            [
                ("coll1", [ "id" .- oidPad "1",
                            "field" .- "val",
                            "field1" .- [dbRef ["coll" .- "coll2", "id" .- pad "0"], d "hey", dbRef ["coll" .- "coll3", "id" .- pad "2"]]
                            ]
                ),
                ("coll2", ["id" .- oidPad "0", "x" .- "lol"]),
                ("coll3", ["id" .- oidPad "2", "y" .- "roflmao"])
            ],
            "coll1",
            [   "id" .- oidPad "1",
                "field" .- "val",
                "field1" .- [d ["id" .- oidPad "0", "x" .- "lol"], d "hey", d ["id" .- oidPad "2", "y" .- "roflmao"]]
            ]
        )
    ]

-- Cross collection dbRef resolution.
testDRCrossColl = TestCase $ mapM_ (\c -> do {(got, verdict) <- f c; want (e3 c) got verdict}) cases2 where
    f c = do
        db <- dbConn
        dropCurrent db
        mapM_ (\a -> insert (setLocation db ["coll" .- fst a]) (snd a)) (e1 c)
        let qdbc = setLocation db ["coll" .- (e2 c)]
        docs <- find qdbc nilDoc >>= resolve db
        let got = Safe.atNote "testDRCrossColl: resolution lost even input docs." docs 0
        if got == toDoc (e3 c) then return (got, True) else return (got, False)

loc31 = ["db" .- "db1", "coll" .- "coll1"]
loc32 = ["db" .- "db2", "coll" .- "coll2"]
-- [([list of docs to insert], location to query, check)]
cases3 = [
        (
            [
                ["location" .- loc31, "id" .- oidPad "1", "a" .- "yo", "z" .- dbRef ["db" .- "db2", "coll" .- "coll2", "id" .- pad "2"]],
                ["location" .- loc32, "id" .- oidPad "2", "b" .- "ahoi"]
            ],
            loc31,
            ["id" .- oidPad "1", "a" .- "yo", "z" .- ["id" .- oidPad "2", "b" .- "ahoi"]]
        )
    ]

-- Cross database dbRef resolution
testDRCrossDb = TestCase $ mapM_ (\c -> do {(got, verdict) <- f c; want (e3 c) got verdict}) cases3 where
    f c = do
        db <- dbConn
        dropDb db loc31
        dropDb db loc32
        mapM_ (\a -> insert db a) (e1 c)
        docs <- find (setLocation db (e2 c)) nilDoc >>= resolve db
        let got = Safe.atNote "testDRCrossDb: resolution lost even input docs." docs 0
        if got == toDoc (e3 c) then return (got, True) else return (got, False)
 
loc41 = ["server" .- "prim", "db" .- "db1", "coll" .- "coll1"]
loc42 = ["server" .- "sec", "db" .- "db2", "coll" .- "coll2"]
cases4 = [
        (
            [
                ["location" .- loc41, "id" .- oidPad "1", "a" .- "yo", "z" .- dbRef ["server" .- "sec", "db" .- "db2", "coll" .- "coll2", "id" .- pad "2"]],
                ["location" .- loc42, "id" .- oidPad "2", "b" .- "ahoi"]
            ],
            loc41,
            ["id" .- oidPad "1", "a" .- "yo", "z" .- ["id" .- oidPad "2", "b" .- "ahoi"]]
        )
    ]

-- This test needs a second server running.
testDRCrossServer = TestCase $ mapM_ (\c -> do {(got, verdict) <- f c; want (e3 c) got verdict}) cases3 where
    f c = do
        db' <- dbConn
        db <- connect db' ["server" .- "127.0.0.1:29000", "serverName" .- "sec"]
        dropDb db loc41
        dropDb db loc42
        mapM_ (\a -> insert db a) (e1 c)
        docs <- find (setLocation db (e2 c)) nilDoc >>= resolve db
        let got = Safe.atNote "testDRCrossServer: resolution lost even input docs." docs 0
        if got == toDoc (e3 c) then return (got, True) else return (got, False)

tests = TestList [
    TestLabel "testDropDb"          testDropDb,
    TestLabel "testDataIntegrity"   testDI,
    TestLabel "testDRCrossColl"     testDRCrossColl,
    TestLabel "testDRCrossDb"       testDRCrossDb,
    TestLabel "testDRCrossServer"   testDRCrossServer
    ]