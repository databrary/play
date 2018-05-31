{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Databrary.Model.AuthorizeTest where

-- import Data.Aeson
-- import Data.Maybe
import Data.Time
import qualified Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Has
-- import Databrary.Model.Audit (MonadAudit)
import Databrary.Model.Authorize
-- import Databrary.Model.Category
-- import Databrary.Model.Container
import Databrary.Model.Party
import Databrary.Model.Party.TypesTest
import Databrary.Model.Permission
-- import Databrary.Model.Release
-- import Databrary.Model.Record
-- import Databrary.Model.Slot
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
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

test_Authorize_examples :: [TestTree]
test_Authorize_examples =
    [
      Test.stepsWithTransaction "superadmin grant admin" $ \step cn2 -> do
        step "Given a superadmin"
        ctxt <-
            runReaderT
                (do
                    Just auth2 <- lookupSiteAuthByEmail False "test@databrary.org"
                    let pid = Id 7
                        ident = fakeIdentSessFromAuth auth2 True
                    pure TestContext {
                                ctxConn = cn2
                            , ctxIdentity = ident
                            , ctxSiteAuth = view ident
                            , ctxPartyId = pid
                            , ctxRequest = defaultRequest
                            })
                TestContext { ctxConn = cn2 }
        step "When the superadmin grants the institution admin access on the db site"
        p <- Gen.sample genCreateInstitutionParty
        authorization1 <-
            runReaderT
                (do
                    created <- addParty p
                    changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionNONE) Nothing created rootParty)
                    -- TODO: what can an institution do on the site, if anything?
                    lookupAuthorization created rootParty
                )
                ctxt
        step "Then we expect the institution to have ADMIN site access, no member privileges"
        authorizeAccess authorization1 @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionNONE }
    , Test.stepsWithTransaction "superadmin grant edit" $ \step cn2 -> do
        -- Note to self: beyond documentation, this a long winded way of testing authorize_view
        step "Given a superadmin and an institution authorized as admin under db site"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt
        step "When the superadmin grants an authorized investigator with edit access on their parent institution"
        aiAcct <- addAuthorizedInvestigator ctxt instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False (accountEmail aiAcct)) ctxtNoIdent
        step "Then we expect the authorized investigator to effectively have edit db site access"
        siteAccess aiAuth @?= Access { accessSite' = PermissionEDIT, accessMember' = PermissionNONE }
    , Test.stepsWithTransaction "authorized investigator grants" $ \step cn2 -> do
        step "Given an authorized investigator"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt
        aiAcct <- addAuthorizedInvestigator ctxt instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False (accountEmail aiAcct)) ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
            aiParty = accountParty aiAcct
        step "When the authorized investigator grants various affiliates access on their lab and/or db site data"
        affAcct1 <- addAffiliate aiCtxt aiParty PermissionNONE PermissionEDIT
        undergradAffAuth <- lookupSiteAuthNoIdent aiCtxt (accountEmail affAcct1)
        affAcct2 <- addAffiliate aiCtxt aiParty PermissionREAD PermissionADMIN
        gradAffAuth <- lookupSiteAuthNoIdent aiCtxt (accountEmail affAcct2)
        affAcct3 <- addAffiliate aiCtxt aiParty PermissionREAD PermissionREAD
        aff1Auth <- lookupSiteAuthNoIdent aiCtxt (accountEmail affAcct3)
        affAcct4 <- addAffiliate aiCtxt aiParty PermissionREAD PermissionEDIT
        aff2Auth <- lookupSiteAuthNoIdent aiCtxt (accountEmail affAcct4)
        step "Then we expect each affiliate to have appropriate db site data and site admin access"
        accessIsEq (siteAccess undergradAffAuth) PermissionNONE PermissionNONE
        accessIsEq (siteAccess gradAffAuth) PermissionREAD PermissionNONE
        accessIsEq (siteAccess aff1Auth) PermissionREAD PermissionNONE
        accessIsEq (siteAccess aff2Auth) PermissionREAD PermissionNONE
    , Test.stepsWithTransaction "authorized investigator authorizes" $ \step cn2 -> do
        step "Given an authorized investigator"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt
        aiAcct <- addAuthorizedInvestigator ctxt instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False (accountEmail aiAcct)) ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
        step "When the AI attempts to authorize some party as a superadmin on db site"
        Just p <- runReaderT (lookupAuthParty ((partyId . partyRow) rootParty)) aiCtxt
        step "Then the attempt fails during the check for privileges on db site party"
        -- guts of checkPermission2, as used by getParty and postAuthorize - <= ADMIN
        partyPermission p @?= PermissionSHARED
    , Test.stepsWithTransaction "affiliate authorizes" (\step cn2 -> do
        step "Given an affiliate (with high priviliges)"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt
        aiAcct <- addAuthorizedInvestigator ctxt instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False (accountEmail aiAcct)) ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
            aiParty = accountParty aiAcct
        affAcct <- addAffiliate aiCtxt aiParty PermissionREAD PermissionADMIN
        gradAffAuth <- lookupSiteAuthNoIdent aiCtxt (accountEmail affAcct)
        let affCtxt = switchIdentity ctxt gradAffAuth False
        step "When affiliate attempts to authorize anybody to any other party"
        Just _ <- runReaderT (lookupAuthParty ((partyId . partyRow . accountParty) affAcct)) affCtxt
        step "Then the attempt fails during the check for privileges on the parent party"
        -- guts of checkPermission2, as used by getParty and postAuthorize - <= ADMIN
        -- FAILING - needs change in postAuthorize
        -- partyPermission p @?= PermissionEDIT
        )
    ]

accessIsEq :: Access -> Permission -> Permission -> Assertion
accessIsEq a site member = a @?= Access { accessSite' = site, accessMember' = member }
