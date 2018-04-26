{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Controller.LoginTest where

import Test.Tasty.HUnit

import Databrary.Controller.Login
import Databrary.Model.Party.Types
-- import Databrary.Model.Party.TypesTest

unit_checkPassword :: Assertion
unit_checkPassword = do
  -- example
  checkPassword "pass1" (blankSiteAuth { accountPasswd = Just "$2b$12$w/ibpcJveMPnP1K0/OObcuggrFzwyzDsnTk1Vdr8Bk6EA9Gue1Bem" })
    @? "expected password passes"
  -- typical
  not (checkPassword "pass1" (blankSiteAuth { accountPasswd = Nothing })) @? "can't login when password not set yet"
  -- edge cases

-- mkSiteAuth :: BS.ByteString -> SiteAuth

blankSiteAuth :: SiteAuth
blankSiteAuth =
    SiteAuth {
          siteAccount = undefined
        , accountPasswd = undefined
        , siteAccess = undefined
        }
