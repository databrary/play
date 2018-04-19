module Databrary.StringUtilTest
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.StringUtil

test_all :: [TestTree]
test_all =
  [ testCase "toCamel-1"
      (toCamel "abc" @?= "Abc")
  , testCase "toCamel-2"
      (toCamel "abc_efg" @?= "AbcEfg")
  , testCase "fromCamel-1"
      (fromCamel "Abc" @?= "abc")
  , testCase "fromCamel-2"
      (fromCamel "AbcEfg" @?= "abc_efg")
  ]
