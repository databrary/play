module Databrary.StringTest
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.String

tests :: TestTree
tests = testGroup "Databrary.String"
  [ testCase "toCamel-1"
      (toCamel "abc" @?= "Abc")
  , testCase "toCamel-2"
      (toCamel "abc_efg" @?= "AbcEfg")
  , testCase "fromCamel-1"
      (fromCamel "Abc" @?= "abc")
  , testCase "fromCamel-2"
      (fromCamel "AbcEfg" @?= "abc_efg")
  ]
