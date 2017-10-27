import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.RangeSet.ParseTest
import qualified Databrary.Model.TimeTest
import qualified Databrary.StringTest
import qualified Databrary.OpsTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit" 
  [ Data.RangeSet.ParseTest.tests
  , Databrary.Model.TimeTest.tests
  , Databrary.StringTest.tests
  , Databrary.OpsTest.tests
  ]

