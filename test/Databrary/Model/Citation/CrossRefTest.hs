{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Citation.CrossRefTest where

import Data.Maybe
import Network.URI
import Test.Tasty.HUnit

import Databrary.Model.Citation.CrossRef

unit_uriHDL :: Assertion
unit_uriHDL =
  -- example
  (uriHDL . fromJust . parseURI) "http://google.com" @?= Nothing
  -- typical
  -- edge cases
