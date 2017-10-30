import Test.Tasty
import Test.Tasty.HUnit

import qualified Databrary.Solr.ServiceTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit" 
  [ Databrary.Solr.ServiceTest.tests
  ]

