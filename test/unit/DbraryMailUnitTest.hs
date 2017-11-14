import Test.Tasty
import Test.Tasty.HUnit

import qualified Databrary.Service.MailTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit" 
  [ Databrary.Service.MailTest.tests
  ]

