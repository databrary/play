{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.ConfigTest
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Store.Config

tests :: TestTree
tests = testGroup "Databrary.Store.Config"
  [ testCase "pathKey-1"
      ((pathKey (Path ["k1", "k2"])) @?= "k1.k2")
  ]
