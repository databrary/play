{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Controller.LoginTest where

import Test.Tasty.HUnit

import Databrary.Controller.Login
import Databrary.Model.Party.Types
-- import Databrary.Model.Party.TypesTest

unit_checkPassword :: Assertion
unit_checkPassword =
  -- example
  checkPassword "pass1" (blankSiteAuth { accountPasswd = Just "pass1" }) @? "expected password passes"
  -- typical
  -- edge cases

-- mkSiteAuth :: BS.ByteString -> SiteAuth

blankSiteAuth :: SiteAuth
blankSiteAuth =
    SiteAuth {
          siteAccount = undefined
        , accountPasswd = undefined
        , siteAccess = undefined
        }
