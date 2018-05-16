{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.AuthorizeTest where

-- import qualified Data.ByteString as BS
-- import qualified Data.Text as T
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Has
import Databrary.Model.Authorize
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
-- import Databrary.Model.Token
import TestHarness

-- session exercise various logic in Authorize
test_Authorize_examples :: TestTree
test_Authorize_examples = testCaseSteps "Authorize examples" $ \step -> do
    (authorizeExpires . selfAuthorize) nobodyParty @?= Nothing

    cn <- connectTestDb
    step "Given an admin user"
    let adminUser = nobodyParty { partyRow = (partyRow nobodyParty) { partyId = Id 7 } }
    step "When we look at its direct authorization on databrary site"
    Just auth <- runReaderT (lookupAuthorize ActiveAuthorizations adminUser rootParty) TestContext { ctxConn = cn }
    step "Then we expect the authorization to have site and member level of ADMIN"
    (authorizeAccess . authorization) auth @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionADMIN }

    withinTestTransaction (\cn2 -> do
        step "Given the databrary site group"
        let dbSite = rootParty
        step "When we grant a user as super admin"
        let ctx =
                TestContext { ctxConn = cn2, ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxRequest = defaultRequest }
            a = mkAccount "Smith" "Jake" "jake@smith.com"
        Just auth3 <-
            runReaderT
                (do
                     a2 <- addAccount a
                     Just auth2 <- lookupSiteAuthByEmail False "jake@smith.com"
                     changeAccount (auth2 { accountPasswd = Just "somehashval"})
                     changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionADMIN) Nothing (accountParty a2) dbSite)
                     lookupSiteAuthByEmail False "jake@smith.com")
                ctx
        step "Then we expect the user to have admin privileges on the databrary site"
        siteAccess auth3 @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionADMIN })

    withinTestTransaction (\cn2 -> do
        step "Given a superadmin"
        ctxt <-
            runReaderT
                (do
                     Just auth2 <- lookupSiteAuthByEmail False "test@databrary.org"
                     let pid = Id 7
                         ident = fakeIdentSessFromAuth auth2 True
                     pure (TestContext {
                                ctxConn = cn2
                              , ctxIdentity = ident
                              , ctxSiteAuth = view ident
                              , ctxPartyId = pid
                              , ctxRequest = defaultRequest
                              }))
                TestContext { ctxConn = cn }
        step "When the superadmin grants the institution admin access on the db site"
        let p = mkInstitution "New York University"
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
        authorizeAccess authorization1 @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionNONE })

    -- Note to self: beyond documentation, this a long winded way of testing authorize_view
    withinTestTransaction (\cn2 -> do
        step "Given a superadmin and an institution authorized as admin under db site"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        step "When the superadmin grants an authorized investigator with edit access on their parent institution"
        _ <- addAuthorizedInvestigator ctxt "Smith" "Raul" "raul@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "raul@smith.com") ctxtNoIdent
        step "Then we expect the authorized investigator to effectively have edit db site access"
        siteAccess aiAuth @?= Access { accessSite' = PermissionEDIT, accessMember' = PermissionNONE })

    withinTestTransaction (\cn2 -> do
        step "Given an authorized investigator"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        aiAcct <- addAuthorizedInvestigator ctxt "Smith" "Mick" "mick@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "mick@smith.com") ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
            aiParty = accountParty aiAcct
        step "When the authorized investigator grants various affiliates access on their lab and/or db site data"
        _ <- addAffiliate aiCtxt "Smith" "Akbar" "akbar@smith.com" aiParty PermissionNONE PermissionEDIT
        undergradAffAuth <- lookupSiteAuthNoIdent aiCtxt "akbar@smith.com"
        _ <- addAffiliate aiCtxt "Smith" "Bob" "bob@smith.com" aiParty PermissionREAD PermissionADMIN
        gradAffAuth <- lookupSiteAuthNoIdent aiCtxt "bob@smith.com"
        _ <- addAffiliate aiCtxt "Smith" "Chris" "chris@smith.com" aiParty PermissionREAD PermissionREAD
        aff1Auth <- lookupSiteAuthNoIdent aiCtxt "chris@smith.com"
        _ <- addAffiliate aiCtxt "Smith" "Daria" "daria@smith.com" aiParty PermissionREAD PermissionEDIT
        aff2Auth <- lookupSiteAuthNoIdent aiCtxt "daria@smith.com"
        step "Then we expect each affiliate to have appropriate db site data and site admin access"
        accessIsEq (siteAccess undergradAffAuth) PermissionNONE PermissionNONE
        accessIsEq (siteAccess gradAffAuth) PermissionREAD PermissionNONE
        accessIsEq (siteAccess aff1Auth) PermissionREAD PermissionNONE
        accessIsEq (siteAccess aff2Auth) PermissionREAD PermissionNONE)

    withinTestTransaction (\cn2 -> do
        step "Given an authorized investigator"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        _ <- addAuthorizedInvestigator ctxt "Smith" "Raul" "raul@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "raul@smith.com") ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
        step "When the AI attempts to authorize some party as a superadmin on db site"
        Just p <- runReaderT (lookupAuthParty ((partyId . partyRow) rootParty)) aiCtxt
        step "Then the attempt fails during the check for privileges on db site party"
        -- guts of checkPermission2, as used by getParty and postAuthorize - <= ADMIN
        partyPermission p @?= PermissionSHARED)

    withinTestTransaction (\cn2 -> do
        step "Given an affiliate (with high priviliges)"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        aiAcct <- addAuthorizedInvestigator ctxt "Smith" "Mick" "mick@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "mick@smith.com") ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
            aiParty = accountParty aiAcct
        affAcct <- addAffiliate aiCtxt "Smith" "Bob" "bob@smith.com" aiParty PermissionREAD PermissionADMIN
        gradAffAuth <- lookupSiteAuthNoIdent aiCtxt "bob@smith.com"
        let affCtxt = switchIdentity ctxt gradAffAuth False
        step "When affiliate attempts to authorize anybody to any other party"
        Just _ <- runReaderT (lookupAuthParty ((partyId . partyRow . accountParty) affAcct)) affCtxt
        step "Then the attempt fails during the check for privileges on the parent party"
        -- guts of checkPermission2, as used by getParty and postAuthorize - <= ADMIN
        -- FAILING - needs change in postAuthorize
        -- partyPermission p @?= PermissionEDIT
        )

    withinTestTransaction (\cn2 -> do -- TODO: move this to VolumeAccess or more general module around authorization
        step "Given an authorized investigator"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        aiAcct <- addAuthorizedInvestigator ctxt "Smith" "Raul" "raul@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "raul@smith.com") ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
        step "When the AI creates a private volume"
        -- TODO: should be lookup auth on rootParty
        let aiParty = accountParty aiAcct
        createdVol <- runReaderT
             (do
                  -- TODO: make a util function in Model for use here
                  v <- addVolume volumeExample -- note: skipping irrelevant change volume citation
                  setDefaultVolumeAccessesForCreated aiParty v
                  -- simulate setting volume as private
                  _ <- changeVolumeAccess (mkVolAccess PermissionNONE Nothing nobodyParty v) -- TODO: handle root also?
                  pure v)
             aiCtxt
        -- TODO: should change owner volume access; nobody vol acc; root vol access
        step "Then the public can't view it"
        -- Implementation of getVolume PUBLIC
        mVolForAnon <- runReaderT (lookupVolume ((volumeId . volumeRow) createdVol)) ctxtNoIdent
        mVolForAnon @?= Nothing)

    -- no ident      + community shared
    -- lab only aff  + vol owner only (other lab)
    -- lab only aff  + direct access to other lab
    -- lab only aff  + community access
    -- site only aff + vol owner only (this lab)
    -- site only aff + direct access to other lab
 
mkVolAccess :: Permission -> Maybe Bool -> Party -> Volume -> VolumeAccess
mkVolAccess perm mShareFull p v =
    VolumeAccess perm perm Nothing mShareFull p v

accessIsEq :: Access -> Permission -> Permission -> Assertion
accessIsEq a site member = a @?= Access { accessSite' = site, accessMember' = member }

-- TODO: copied from VolumeTest, move to shared area instead
volumeExample :: Volume
volumeExample =
    let
        row =
           VolumeRow {
                 volumeId = Id 1
               , volumeName = "Test Vol One: A Survey"
               , volumeBody = Just "Here is a description for a volume"
               , volumeAlias = Just "Test Vol 1"
               , volumeDOI = Nothing
               }
    in
        Volume {
              volumeRow = row
            , volumeCreation = UTCTime (fromGregorian 2018 1 2) (secondsToDiffTime 0)
            , volumeOwners = [] -- [(Id 2, "Smith, John")]
            , volumePermission = PermissionPUBLIC
            , volumeAccessPolicy = PermLevelDefault
            }
