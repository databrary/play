{-# LANGUAGE OverloadedStrings #-}
module Databrary.EZID.ANVLTest
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.ByteString.Builder (toLazyByteString)
import Data.Attoparsec.ByteString (parseOnly)
import Databrary.EZID.ANVL

tests :: TestTree
tests = testGroup "Databrary.EZID.ANVL"
  [ testCase "encode-1"
      (toLazyByteString (encode [("key", "val")]) @?= "key: val\n")
  , testCase "parse-1"
      (parseOnly parse "key: val\n" @?= Right [("key", "val")])
  ] 
