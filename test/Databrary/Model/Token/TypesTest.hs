{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Token.TypesTest where

import Data.Time
-- import Test.Tasty.HUnit

import Databrary.Model.Token.Types
import Databrary.Model.Id.Types

token1 :: Token
token1 =
    Token {
          tokenId = Id "tk1"
        , tokenExpires = UTCTime (fromGregorian 2017 1 2) (secondsToDiffTime 0)
        }
