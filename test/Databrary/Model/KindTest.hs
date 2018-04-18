{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Databrary.Model.KindTest
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Kind

test_kindOf :: TestTree
test_kindOf = testGroup "kindOf"
   [ testCase "example"
         ((kindOf SomeObject :: String) @?= "someobject")
   ]

data SomeObject = SomeObject

instance Kinded SomeObject where
    kindOf _ = "someobject"
