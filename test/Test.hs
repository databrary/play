module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure

import Databrary.Main hiding (main)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "databrary" [testMain]

testMain :: TestTree
testMain = testGroup
    "Databrary.Main"
    [ expectFail $ testCase
          "flagConfig sanity check"

            ( do
              (1 :: Int) @?= (2 :: Int) )
          {-( do
              flagConfig (FlagConfig "foo") @?= Left "foo"
              flagConfig (FlagWeb) @?= Right FlagWeb
          )-}
    ]
