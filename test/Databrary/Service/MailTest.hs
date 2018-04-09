{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.MailTest
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Service.Mail

tests :: TestTree
tests = testGroup "Databrary.Service.Mail"
  [ testCase "wrapText-1"
       (wrapText 10 "two lines of stuff" @?= "two lines\nof stuff\n")
  ]
