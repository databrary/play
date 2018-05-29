{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Databrary.ModelTest where

import Data.Aeson
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Has
import Databrary.Model.Audit (MonadAudit)
import Databrary.Model.Authorize
import Databrary.Model.Category
import Databrary.Model.Container
import Databrary.Model.Identity (MonadHasIdentity)
import Databrary.Model.Measure
import Databrary.Model.Metric
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Record
import Databrary.Model.Slot
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
import Databrary.Service.DB (DBConn, MonadDB)
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

setSiteAuthFromIdent :: TestContext -> TestContext
setSiteAuthFromIdent c = c { ctxSiteAuth = view (ctxIdentity c) }

setDefaultRequest :: TestContext -> TestContext
setDefaultRequest c = c { ctxRequest = defaultRequest }

-- 2 = superadmin grant
-- 3 = superadmin grant edit
-- 4 = ai grant
-- 5 = ai authorize
-- 6 = aff authorize
test_7 :: TestTree
test_7 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the AI creates a private volume"
    -- TODO: should be lookup auth on rootParty
    vid <- runReaderT
         (do
              v <- addVolumeSetPrivate volumeExample aiAcct
              pure ((volumeId . volumeRow) v))
         aiCtxt
    step "Then the public can't view it"
    -- Implementation of getVolume PUBLIC
    mVolForAnon <- runWithNoIdent cn2 (lookupVolume vid)
    mVolForAnon @?= Nothing

-- 8 = ai lab a, lab b access
-- 9 = aff site access only
-- 10 = ai lab a, ai lab b vol access

----- container ----
test_11 :: TestTree
test_11 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the AI creates a private volume with a fully released container"
    -- TODO: should be lookup auth on rootParty
    cid <- runReaderT
         (do
              v <- addVolumeSetPrivate volumeExample aiAcct
              makeAddContainer v (Just ReleasePUBLIC) Nothing)
         aiCtxt
    step "Then the public can't view the container"
    -- Implementation of getSlot PUBLIC
    mSlotForAnon <- runWithNoIdent cn2 (lookupSlotByContainerId cid)
    isNothing mSlotForAnon @? "expected slot lookup to find nothing"

test_12 :: TestTree
test_12 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator's created public volume with a container released at Excerpts level"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    -- TODO: should be lookup auth on rootParty
    cid <- runReaderT
         (do
              v <- addVolumeWithAccess volumeExample aiAcct
              makeAddContainer v (Just ReleaseEXCERPTS) (Just (someDay 2017)))
         aiCtxt
    step "When the public attempts to view the container"
    -- Implementation of getSlot PUBLIC
    Just slotForAnon <- runWithNoIdent cn2 (lookupSlotByContainerId cid)
    step "Then the public can't see protected parts like the detailed test date"
    (encode . getContainerDate . slotContainer) slotForAnon @?= "2017"

someDay :: Integer -> Day
someDay yr = fromGregorian yr 1 2

----- record ---
test_13 :: TestTree
test_13 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator's created private volume with a record not attached to a container"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    -- TODO: should be lookup auth on rootParty
    rid <- runReaderT
         (do
              v <- addVolumeSetPrivate volumeExample aiAcct
              addParticipantRecordWithMeasures v [])
         aiCtxt
    step "When the public attempts to view the record"
    -- Implementation of getRecord PUBLIC
    mRcrd <- runWithNoIdent cn2 (lookupRecord rid)
    step "Then the public can't"
    isNothing mRcrd @? "Expected failure to retrieve record from restricted volume"

test_14 :: TestTree
test_14 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator's created public volume with a record not attached to a container"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    -- TODO: should be lookup auth on rootParty
    rid <- runReaderT
         (do
              v <- addVolumeWithAccess volumeExample aiAcct
              addParticipantRecordWithMeasures v [someBirthdateMeasure, someGenderMeasure])
         aiCtxt
    step "When the public attempts to view the record"
    -- Implementation of getRecord PUBLIC
    Just rcrdForAnon <- runWithNoIdent cn2 (lookupRecord rid)
    step "Then the public can't see the restricted measures like birthdate"
    (participantMetricBirthdate `notElem` (fmap measureMetric . getRecordMeasures) rcrdForAnon) @? "Expected birthdate to be removed"

lookupSlotByContainerId :: (MonadDB c m, MonadHasIdentity c m) => Id Container -> m (Maybe Slot)
lookupSlotByContainerId cid = lookupSlot (containerSlotId cid)

setVolumePrivate :: (MonadAudit c m) => Volume -> m ()
setVolumePrivate v = do
    void (changeVolumeAccess (mkVolAccess PermissionNONE Nothing nobodyParty v))
    void (changeVolumeAccess (mkVolAccess PermissionNONE Nothing rootParty v))

runWithNoIdent :: DBConn -> ReaderT TestContext IO a -> IO a
runWithNoIdent cn rdr = runReaderT rdr ((mkDbContext cn) { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded })

addAuthorizedInvestigatorWithInstitution' :: DBConn -> IO (Account, TestContext)
addAuthorizedInvestigatorWithInstitution' c =
    addAuthorizedInvestigatorWithInstitution c "test@databrary.org" "New York University" "raul@smith.com"

-- modeled after createContainer
makeAddContainer :: (MonadAudit c m) => Volume -> Maybe Release -> Maybe Day -> m (Id Container)
makeAddContainer v mRel mDate = do
    c <- addContainer (mkContainer v mRel mDate)
    pure ((containerId . containerRow) c)

mkContainer :: Volume -> Maybe Release -> Maybe Day -> Container
mkContainer v mRel mDate = -- note: modeled after create container
    let
        c = blankContainer v
    in
        c { containerRelease = mRel
          , containerRow = (containerRow c) { containerDate = mDate }
          }

addParticipantRecordWithMeasures :: (MonadAudit c m) => Volume -> [Record -> Measure] -> m (Id Record)
addParticipantRecordWithMeasures v mkMeasures = do
    r <- addRecord (mkParticipantRecord v)
    forM_
        mkMeasures
        (\mk -> changeRecordMeasure (mk r))
    pure ((recordId . recordRow) r)

-- TODO: remove from authorizetest
addVolumeWithAccess :: MonadAudit c m => Volume -> Account -> m Volume
addVolumeWithAccess v a = do
    v' <- addVolume v -- note: skipping irrelevant change volume citation
    setDefaultVolumeAccessesForCreated (accountParty a) v'
    pure v'

addVolumeSetPrivate :: (MonadAudit c m) => Volume -> Account -> m Volume
addVolumeSetPrivate v a = do
    v' <- addVolumeWithAccess v a
    setVolumePrivate v'
    pure v'

-- TODO: remove from authorizetest
mkVolAccess :: Permission -> Maybe Bool -> Party -> Volume -> VolumeAccess
mkVolAccess perm mShareFull p v =
    VolumeAccess perm perm Nothing mShareFull p v

someBirthdateMeasure :: Record -> Measure
someBirthdateMeasure r = Measure r participantMetricBirthdate "1990-01-02"

someGenderMeasure :: Record -> Measure
someGenderMeasure r = Measure r participantMetricGender "Male"

mkParticipantRecord :: Volume -> Record
mkParticipantRecord vol =  -- note: modeled after create record
    blankRecord participantCategory vol

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
