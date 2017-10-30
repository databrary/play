import Test.Tasty
import Test.Tasty.HUnit

import qualified Databrary.Service.PasswdTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit" 
  [ Databrary.Service.PasswdTest.tests
  ]

