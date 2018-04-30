{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Action.RequestTest where

import Network.Wai
import Test.Tasty.HUnit

import Databrary.Action.Request

unit_isDatabraryClient :: Assertion
unit_isDatabraryClient = do
    -- example
    isDatabraryClient defaultRequest @?= False
    -- typical
    isDatabraryClient (defaultRequest { requestHeaders = [("x-requested-with", "DatabraryClient")] }) @?= True
