module Databrary.JSONTest where

import Test.Tasty.HUnit

import Databrary.JSON

unit_kvObjectOrEmpty :: Assertion
unit_kvObjectOrEmpty = do
  -- example
  "k1" `kvObjectOrEmpty` (Nothing :: Maybe Int) @?= ([] :: [Pair])
  "k1" `kvObjectOrEmpty` (Just 3 :: Maybe Int) @?= ([("k1", Number 3)] :: [Pair])
