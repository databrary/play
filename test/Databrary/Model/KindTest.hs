{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Databrary.Model.KindTest
where

import Test.Tasty.HUnit

import Databrary.Model.Kind

unit_kindOf :: Assertion
unit_kindOf = 
    -- example
    (kindOf SomeObject :: String) @?= "someobject"

data SomeObject = SomeObject

instance Kinded SomeObject where
    kindOf _ = "someobject"
