{-# LANGUAGE OverloadedStrings #-}
module Tests.Hakit.Spice where

import Hakit.Spice
import Hakit
import Test.HUnit

want what verdict = assertBool (show what) verdict

cases1 = [
    -- Id
    (div' [cat "id" "testId"] [],     "#testId",                    True),
    (div' [cat "id" "testId"] [],     "#testId1",                   False),
    -- Class
    (div' [cat "class" "testC"] [],     ".testC",                   True),
    (div' [cat "class" "testC"] [],     ".test1C",                  False),
    (div' [cat "class" "cl1 cl2 cl3"] [],     ".cl1",               True),
    (div' [cat "class" "cl1 cl2 cl3"] [],     ".cl2",               True),
    (div' [cat "class" "cl1 cl2 cl3"] [],     ".cl3",               True),
    (div' [cat "class" "cl1 cl2 cl3"] [],     ".cl4",               False),
    -- Attributes
    (div' [cat "href" "whatever"] [],   "[href=\"whatever\"]",      True),
    (div' [cat "href" "whatever"] [],   "[href=\"whaver\"]",        False),
    (div' [cat "href" "whatever"] [],   "[href=whatever]",          True),
    (div' [cat "href" "whatever"] [],   "[href=whaver]",            False),
    (div' [cat "href" "whatever"] [],   "[href^=\"what\"]",         True),
    (div' [cat "href" "whatever"] [],   "[href^=\"what1\"]",        False),
    (div' [cat "href" "whatever"] [],   "[href^=what]",             True),
    (div' [cat "href" "whatever"] [],   "[href^=what1]",            False),
    (div' [cat "href" "whatever"] [],   "[href$=\"ever\"]",         True),
    (div' [cat "href" "whatever"] [],   "[href$=\"ever1\"]",        False),
    (div' [cat "href" "whatever"] [],   "[href$=ever]",             True),
    (div' [cat "href" "whatever"] [],   "[href$=ever1]",            False),
    -- Composite
    (div' [cat "id" "tid", cat "class" "tc"] [],   "#tid.tc",       True),
    (div' [cat "id" "tid", cat "class" "tc"] [],   "#tid.tc1",      False),
    (div' [cat "id" "tid", cat "class" "tc"] [],   "#tid1.tc",      False),
    (div' [cat "id" "tid", cat "class" "tc"] [],   ".tc#tid",       True),
    (div' [cat "id" "tid", cat "class" "tc"] [],   ".tc1#tid",      False),
    (div' [cat "id" "tid", cat "class" "tc"] [],   ".tc#tid1",      False)
    ]

t c = TestCase $ mapM_ (\c -> want (e1 c, e2 c) (matches (e2 c) (e1 c) == e3 c)) c

testMatches = t cases1

cases2 = [
    (
        div' [cat "id" "t1"] [
            div' [cat "id" "t2", cat "class" "tclass"] []
        ], [("#t2.tclass", 1), (".tclass#t2", 1)]
    ),
    (
        div' [cat "id" "t1", cat "class" "c1"] [
            div' [cat "id" "t2", cat "class" "c2"] [
                div' [cat "id" "t3", cat "class" "c3"] []
            ]
        ], [("#t1 #t2 #t3", 1), ("#t1 #t3", 1), ("#t1 > #t3", 0), ("#t1.c1 #t3.c3", 1), ("#t1.c11 #t3.c33", 0)]
    ),
    (
        div' [cat "id" "t1", cat "class" "c1"] [
            div' [cat "id" "t2", cat "class" "c2"] [
                div' [cat "id" "t3", cat "class" "c3"] [
                    div' [cat "id" "t4", cat "class" "c4"] []
                ]
            ],
            div' [cat "id" "t21", cat "class" "c21"] [
                div' [cat "id" "t31", cat "class" "c31"] [
                    div' [cat "id" "t41", cat "class" "c41"] []
                ]
            ]
        ], [("#t1 #t4", 1)]
    )
    ]

testSelect = TestCase $ mapM_ (\(tag, checks) -> mapM_ (\c -> want (c, tag) ((length $ select (fst c) tag) == snd c)) checks) cases2

tests = TestList [
    TestLabel "testMatches" testMatches,
    TestLabel "testSelect" testSelect
    ]