{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.AuthorizeTest where

import Test.Tasty.HUnit

import Databrary.Model.Authorize
import Databrary.Model.Party

unit_selfAuthorize :: Assertion
unit_selfAuthorize = do
  -- example
  (authorizeExpires . selfAuthorize) nobodyParty @?= Nothing
  -- typical
  -- edge cases
