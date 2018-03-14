module Main (main) where

import Test.Tasty
import qualified Databrary.HTTP.Form.DeformTest
import qualified Databrary.Model.RecordTest
import qualified Data.Csv.ContribTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "databrary"
    [ Databrary.HTTP.Form.DeformTest.tests
    , Databrary.Model.RecordTest.tests
    , Data.Csv.ContribTest.tests
    ]
