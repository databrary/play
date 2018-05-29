{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.ModelTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Authorize
import Databrary.Model.Party
import Databrary.Model.Permission
import TestHarness as Test

test_1 :: TestTree
test_1 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given the databrary site group"
    let dbSite = rootParty
    step "When we grant a user as super admin"
    let ctx = setDefaultRequest (setIdentityNotNeeded (mkDbContext cn2))
    Just auth3 <-
        runReaderT
            (do
                a2 <- addAccount (mkAccountSimple "jake@smith.com")
                Just auth2 <- lookupSiteAuthByEmail False "jake@smith.com"
                changeAccount (auth2 { accountPasswd = Just "somehashval"})
                changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionADMIN) Nothing (accountParty a2) dbSite)
                lookupSiteAuthByEmail False "jake@smith.com")
            ctx
    step "Then we expect the user to have admin privileges on the databrary site"
    let acc = siteAccess auth3
    accessSite' acc @?= PermissionADMIN
    accessMember' acc @?= PermissionADMIN

setIdentityNotNeeded :: TestContext -> TestContext
setIdentityNotNeeded c = c { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1) }

setDefaultRequest :: TestContext -> TestContext
setDefaultRequest c = c { ctxRequest = defaultRequest }
