import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.RangeSet.ParseTest
import qualified Databrary.StringTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit" 
  [ Data.RangeSet.ParseTest.tests
  , Databrary.StringTest.tests
  ]

