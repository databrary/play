module Main (main) where

import Test.Tasty
import qualified Databrary.HTTP.Form.DeformTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "databrary"
    [ Databrary.HTTP.Form.DeformTest.tests
    ]
