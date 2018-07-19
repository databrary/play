{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Controller.LoginTest where

import Test.Tasty
import Test.Tasty.HUnit

import Controller.Login
import Model.Party.Types
-- import Model.Party.TypesTest

-- Takes longer than a second, because that's the whole point of password
-- algorithms.
test_checkPassword :: TestTree
test_checkPassword = localOption (mkTimeout (2 * 10^(6::Int))) $ testCase "checkPassword" $ do
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
