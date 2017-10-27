import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.RangeSet.ParseTest
import qualified Databrary.EZID.ANVLTest
import qualified Databrary.Model.TimeTest
import qualified Databrary.StringTest
import qualified Databrary.OpsTest
import qualified Databrary.HTTP.RequestTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit" 
  [ Data.RangeSet.ParseTest.tests
  , Databrary.EZID.ANVLTest.tests
  , Databrary.Model.TimeTest.tests
  , Databrary.StringTest.tests
  , Databrary.OpsTest.tests
  , Databrary.HTTP.RequestTest.tests
  ]

