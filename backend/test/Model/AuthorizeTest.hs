{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Model.AuthorizeTest where

import Test.Tasty.HUnit

import Model.Authorize
import Model.Party
import Model.Permission
import TestHarness as Test

-- session exercise various logic in Authorize
unit_Authorize_examples :: Assertion
unit_Authorize_examples = do
    (authorizeExpires . selfAuthorize) nobodyParty @?= Nothing
    -- lookupAuthorize
    cn <- connectTestDb
    Just auth <- Test.runContextReaderT cn (lookupAuthorize ActiveAuthorizations (mockParty predefinedSiteAdminId) rootParty)
    (authorizeAccess . authorization) auth @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionADMIN }

predefinedSiteAdminId :: Id Party
predefinedSiteAdminId = Id 7

mockParty :: Id Party -> Party
mockParty pid =
    nobodyParty { partyRow = (partyRow nobodyParty) { partyId = pid } }
