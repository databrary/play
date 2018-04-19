{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.PasswdTest
where

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import Databrary.Service.Passwd

test_all :: [TestTree]
test_all =
  [ ignoreTest -- "nix-build can't find dictionary file because it builds in an isolated directory"
        (testCase "passwdCheck-1" (do
             p <- initPasswd
             mErr <- passwdCheck "pass" "user" "john" p
             (mErr @?= Just "it is too short")))
  ]
