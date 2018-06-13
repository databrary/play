{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.IdentityTest where

import Test.Tasty.HUnit

-- import Databrary.Model.Identity
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Permission
import TestHarness

-- A session driving functions exposed from Identity
unit_Identity_examples :: Assertion
unit_Identity_examples = do
    -- guts of retrieving a site auth, used for creating a session upon login
    cn <- connectTestDb
    let ctxt = TestContext { ctxConn = Just cn }
    Just auth <- runReaderT (lookupSiteAuthByEmail False "test@databrary.org") ctxt
    let p = (accountParty . siteAccount) auth
    partyRow p @?=
        (partyRow blankParty) {
              partyId = Id 7
            , partySortName = "Tesla"
            , partyPreName = Just "Testarosa"
            , partyAffiliation = Just "Carnegie Melon"
            }
    (accountEmail . siteAccount) auth @?= "test@databrary.org"
    -- passwd different on different machines?
    -- accountPasswd auth @?= Just "$2b$12$SRKLVEaeWeLZB50Ow4CleuL1NYTKOM7P0S5CIpttNXPzN8rgcMgDW"
    siteAccess auth @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionADMIN }
    -- TODO: explain the values below
    partyPermission p @?= PermissionADMIN
    partyAccess p @?= Just (Access { accessSite' = PermissionADMIN, accessMember' = PermissionADMIN })

    -- login as test@databrary.org using create session
    -- use lookupSession to retrieve the ident

    -- login as test@databrary.org using create session + setsigned cookie
    -- use determine identity to retrieve the ident

