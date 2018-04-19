module Databrary.OpsTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Ops

test_all :: [TestTree]
test_all =
    [ testCase "rightJust-1" (rightJust (Right 10) @?= (Just 10 :: Maybe Int))
    , testCase "rightJust-2" (rightJust (Left ()) @?= (Nothing :: Maybe Int))
    , testCase
        "mergeBy-1"
        (mergeBy compare [1, 3] [2, 4] @?= ([1, 2, 3, 4] :: [Int]))
    , testCase
        "groupTuplesBy-1"
        (groupTuplesBy (==) [(True, 1), (True, 2), (False, 3)]
            @?= ([(True, [1, 2]), (False, [3])] :: [(Bool, [Int])])
        )
    ]
