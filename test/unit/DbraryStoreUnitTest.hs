import Test.Tasty
import Test.Tasty.HUnit

import qualified Databrary.FilesTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit" 
  [ Databrary.FilesTest.tests
  ]

