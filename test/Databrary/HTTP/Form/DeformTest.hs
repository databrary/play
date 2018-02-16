{-# LANGUAGE OverloadedStrings, TypeFamilies, ScopedTypeVariables #-}
module Databrary.HTTP.Form.DeformTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.HTTP.Form.Deform

tests :: TestTree
tests = testGroup "Databrary.HTTP.Form.DeformTest"
    [ testCase "textInteger-1" (textInteger "1" @?= Right (1::Int))
    ]
