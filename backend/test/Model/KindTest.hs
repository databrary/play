{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Model.KindTest
where

import Test.Tasty.HUnit

import Model.Kind

unit_kindOf :: Assertion
unit_kindOf =
    -- example
    (kindOf SomeObject :: String) @?= "someobject"

data SomeObject = SomeObject

instance Kinded SomeObject where
    kindOf _ = "someobject"
