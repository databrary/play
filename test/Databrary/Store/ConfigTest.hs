{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.ConfigTest
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Store.Config

test_all :: TestTree
test_all = testGroup "all"
  [ testCase "pathKey-1"
      ((pathKey (Path ["k1", "k2"])) @?= "k1.k2")
  ]
