import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.RangeSet.ParseTest 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit" 
  [ Data.RangeSet.ParseTest.tests
  ]

