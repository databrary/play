import Test.Tasty
import Test.Tasty.HUnit

import qualified Databrary.Store.ConfigTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit" 
  [ Databrary.Store.ConfigTest.tests
  ]

