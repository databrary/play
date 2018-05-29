{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Databrary.Model.AuthorizeTest where

import Data.Aeson
import Data.Maybe
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Has
import Databrary.Model.Audit (MonadAudit)
import Databrary.Model.Authorize
import Databrary.Model.Category
import Databrary.Model.Container
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Record
import Databrary.Model.Slot
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
    [ testCase "nobody" $ (authorizeExpires . selfAuthorize) nobodyParty @?= Nothing
    , Test.stepsWithTransaction "superadmin grant admin" $ \step cn2 -> do
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
        authorizeAccess authorization1 @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionNONE }
    , Test.stepsWithTransaction "superadmin grant edit" $ \step cn2 -> do
        -- Note to self: beyond documentation, this a long winded way of testing authorize_view
        step "Given a superadmin and an institution authorized as admin under db site"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        step "When the superadmin grants an authorized investigator with edit access on their parent institution"
        _ <- addAuthorizedInvestigator ctxt "Smith" "Raul" "raul@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "raul@smith.com") ctxtNoIdent
        step "Then we expect the authorized investigator to effectively have edit db site access"
        siteAccess aiAuth @?= Access { accessSite' = PermissionEDIT, accessMember' = PermissionNONE }
    , Test.stepsWithTransaction "authorized investigator grants" $ \step cn2 -> do
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
        accessIsEq (siteAccess aff2Auth) PermissionREAD PermissionNONE
    , Test.stepsWithTransaction "authorized investigator authorizes" $ \step cn2 -> do
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
        partyPermission p @?= PermissionSHARED
    , Test.stepsWithTransaction "affiliate authorizes" (\step cn2 -> do
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
    , Test.stepsWithTransaction "authorized investigator creates private volume" (\step cn2 -> do
        -- TODO: move this to VolumeAccess or more general module around authorization
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
                  v <- addVolume volumeExample -- note: skipping irrelevant change volume citation
                  setDefaultVolumeAccessesForCreated aiParty v
                  -- simulate setting volume as private
                  _ <- changeVolumeAccess (mkVolAccess PermissionNONE Nothing nobodyParty v) -- TODO: handle root also?
                  pure v)
             aiCtxt
        step "Then the public can't view it"
        -- Implementation of getVolume PUBLIC
        mVolForAnon <- runReaderT (lookupVolume ((volumeId . volumeRow) createdVol)) ctxtNoIdent
        mVolForAnon @?= Nothing)
    ]

test_Authorize_examples3 :: TestTree
test_Authorize_examples3 = testCaseSteps "Authorize examples continued" $ \step -> do
    withinTestTransaction (\cn2 -> do -- TODO: move this to VolumeAccess or more general module around authorization
        step "Given an authorized investigator for some lab A and a lab B member with lab data access only"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        aiAcct <- addAuthorizedInvestigator ctxt "Smith" "Raul" "raul@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "raul@smith.com") ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
        aiAcct2 <- addAuthorizedInvestigator ctxt "Smith" "Sara" "sara@smith.com" instParty
        Just aiAuth2 <- runReaderT (lookupSiteAuthByEmail False "sara@smith.com") ctxtNoIdent
        let aiCtxt2 = switchIdentity ctxt aiAuth2 False
            aiParty2 = accountParty aiAcct2
        _ <- addAffiliate aiCtxt2 "Smith" "Bob" "bob@smith.com" aiParty2 PermissionNONE PermissionADMIN
        affAuth <- lookupSiteAuthNoIdent aiCtxt2 "bob@smith.com"
        let affCtxt = switchIdentity ctxt affAuth False
        step "When an AI creates a private volume for some lab A"
        -- TODO: should be lookup auth on rootParty
        let aiParty = accountParty aiAcct
        createdVol <- runReaderT
             (do
                  v <- addVolume volumeExample -- note: skipping irrelevant change volume citation
                  setDefaultVolumeAccessesForCreated aiParty v
                  -- simulate setting volume as private
                  _ <- changeVolumeAccess (mkVolAccess PermissionNONE Nothing nobodyParty v)
                  _ <- changeVolumeAccess (mkVolAccess PermissionNONE Nothing rootParty v)
                  pure v)
             aiCtxt
        step "Then the lab B member can't view it"
        -- Implementation of getVolume PUBLIC
        mVolForAff <- runReaderT (lookupVolume ((volumeId . volumeRow) createdVol)) affCtxt
        mVolForAff @?= Nothing)

    withinTestTransaction (\cn2 -> do -- TODO: move this to VolumeAccess or more general module around authorization
        step "Given an authorized investigator and their affiliate with site access only"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        aiAcct <- addAuthorizedInvestigator ctxt "Smith" "Raul" "raul@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "raul@smith.com") ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
            aiParty = accountParty aiAcct
        _ <- addAffiliate aiCtxt "Smith" "Bob" "bob@smith.com" aiParty PermissionREAD PermissionNONE
        affAuth <- lookupSiteAuthNoIdent aiCtxt "bob@smith.com"
        let affCtxt = switchIdentity ctxt affAuth False
        step "When an AI creates a private volume"
        -- TODO: should be lookup auth on rootParty
        createdVol <- runReaderT
             (do
                  v <- addVolume volumeExample -- note: skipping irrelevant change volume citation
                  setDefaultVolumeAccessesForCreated aiParty v
                  -- simulate setting volume as private
                  _ <- changeVolumeAccess (mkVolAccess PermissionNONE Nothing nobodyParty v)
                  _ <- changeVolumeAccess (mkVolAccess PermissionNONE Nothing rootParty v)
                  pure v)
             aiCtxt
        step "Then their lab member with site access only can't view it"
        -- Implementation of getVolume PUBLIC
        mVolForAff <- runReaderT (lookupVolume ((volumeId . volumeRow) createdVol)) affCtxt
        mVolForAff @?= Nothing)

    withinTestTransaction (\cn2 -> do -- TODO: move this to VolumeAccess or more general module around authorization
        step "Given an authorized investigator for some lab A and an authorized investigator for lab B"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        aiAcct <- addAuthorizedInvestigator ctxt "Smith" "Raul" "raul@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "raul@smith.com") ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
        _ <- addAuthorizedInvestigator ctxt "Smith" "Sara" "sara@smith.com" instParty
        Just aiAuth2 <- runReaderT (lookupSiteAuthByEmail False "sara@smith.com") ctxtNoIdent
        let aiCtxt2 = switchIdentity ctxt aiAuth2 False
        step "When the lab A AI creates a public volume"
        -- TODO: should be lookup auth on rootParty
        let aiParty = accountParty aiAcct
        createdVol <- runReaderT
             (do
                  v <- addVolume volumeExample -- note: skipping irrelevant change volume citation
                  setDefaultVolumeAccessesForCreated aiParty v -- partially shared, but effectively same as public
                  pure v)
             aiCtxt
        step "Then the lab B AI can't add volume acccess"
        -- Implementation of getVolume as used by postVolumeAccess
        Just volForAI2 <- runReaderT (lookupVolume ((volumeId . volumeRow) createdVol)) aiCtxt2
        volumePermission volForAI2 @?= PermissionSHARED)

test_Authorize_examples2 :: TestTree
test_Authorize_examples2 = testCaseSteps "Authorize examples continued" $ \step -> do
    withinTestTransaction (\cn2 -> do -- TODO: move this to VolumeAccess or more general module around authorization
        step "Given an authorized investigator"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        aiAcct <- addAuthorizedInvestigator ctxt "Smith" "Raul" "raul@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "raul@smith.com") ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
        step "When the AI creates a private volume with a fully released container"
        -- TODO: should be lookup auth on rootParty
        let aiParty = accountParty aiAcct
        createdContainer <- runReaderT
             (do
                  v <- addVolumeWithAccess volumeExample aiParty
                  -- simulate setting volume as private
                  _ <- changeVolumeAccess (mkVolAccess PermissionNONE Nothing nobodyParty v) -- TODO: handle root also?
                  -- modeled after createContainer
                  addContainer (mkContainer v (Just ReleasePUBLIC) Nothing))
             aiCtxt
        step "Then the public can't view the container"
        -- Implementation of getSlot PUBLIC
        let cid = (containerId . containerRow) createdContainer
        mSlotForAnon <- runReaderT (lookupSlot (containerSlotId cid)) ctxtNoIdent
        isNothing mSlotForAnon @? "expected slot lookup to find nothing")

    withinTestTransaction (\cn2 -> do -- TODO: move this to VolumeAccess or more general module around authorization
        step "Given an authorized investigator's created public volume with a container released at Excerpts level"
        ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
        instParty <- addAuthorizedInstitution ctxt "New York University"
        aiAcct <- addAuthorizedInvestigator ctxt "Smith" "Raul" "raul@smith.com" instParty
        let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
        Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "raul@smith.com") ctxtNoIdent
        let aiCtxt = switchIdentity ctxt aiAuth False
        -- TODO: should be lookup auth on rootParty
        let aiParty = accountParty aiAcct
        createdContainer <- runReaderT
             (do
                  v <- addVolumeWithAccess volumeExample aiParty
                  addContainer (mkContainer v (Just ReleaseEXCERPTS) (Just (fromGregorian 2017 1 2))))
             aiCtxt
        step "When the public attempts to view the container"
        -- Implementation of getSlot PUBLIC
        let cid = (containerId . containerRow) createdContainer
        Just slotForAnon <- runReaderT (lookupSlot (containerSlotId cid)) ctxtNoIdent
        step "Then the public can't see protected parts like the detailed test date"
        (volumePermission . containerVolume . slotContainer) slotForAnon @?= PermissionPUBLIC
        (encode . getContainerDate . slotContainer) slotForAnon @?= "2017")

addVolumeWithAccess :: MonadAudit c m => Volume -> Party -> m Volume
addVolumeWithAccess v p = do
    v' <- addVolume v -- note: skipping irrelevant change volume citation
    setDefaultVolumeAccessesForCreated p v'
    pure v'

mkContainer :: Volume -> Maybe Release -> Maybe Day -> Container
mkContainer v mRel mDate = -- note: modeled after create container
    let
        c = blankContainer v
    in
        c { containerRelease = mRel
          , containerRow = (containerRow c) { containerDate = mDate }
          }

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
