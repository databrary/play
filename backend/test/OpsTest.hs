module OpsTest where

import Test.Tasty
import Test.Tasty.HUnit

import Ops

-- doctest style examples
unit_Ops_examples :: Assertion
unit_Ops_examples = do
    -- TODO: delete Ops module
    True `thenUse` ("abc" :: String) @?= Just "abc"
    False `thenUse` ("abc" :: String) @?= Nothing
    ("abc" :: String) `useWhen` True @?= Just "abc"
    True `unlessUse` (10 :: Integer) @?= Nothing
    True `thenReturn` (Just (20 :: Integer)) @?= Just (Just 20)
    True `unlessReturn` (Just (20 :: Integer)) @?= Just Nothing
    fromMaybeM ["abc" :: String] (Just "efg") @?= ["efg"]
    flatMapM (\v -> [Just v]) (Just ("abc" :: String))  @?= [Just "abc"]

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
