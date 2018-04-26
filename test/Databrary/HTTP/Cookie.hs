{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.HTTP.Cookie where

import Data.Time
import Test.Tasty.HUnit

import Databrary.HTTP.Cookie

unit_setSignedCookie :: Assertion
unit_setSignedCookie = do
  pure ()
  -- example
  -- hdr <- setSignedCookie "cname" "cval" (UTCTime (fromGregorian 2017 1 12) (secondsToDiffTime 0))
  -- typical
  -- edge cases
