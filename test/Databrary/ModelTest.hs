{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Databrary.ModelTest where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Maybe
import Data.Time
import Hedgehog.Gen as Gen
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Has
import Databrary.Model.Audit (MonadAudit)
import Databrary.Model.Authorize
import Databrary.Model.Category
import Databrary.Model.Container
import Databrary.Model.Container.TypesTest
import Databrary.Model.Identity (MonadHasIdentity)
import Databrary.Model.Measure
import Databrary.Model.Metric
import Databrary.Model.Party
import Databrary.Model.Party.TypesTest
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Record
import Databrary.Model.Record.TypesTest
import Databrary.Model.Slot
import Databrary.Model.Volume
import Databrary.Model.Volume.TypesTest
import Databrary.Model.VolumeAccess
import Databrary.Model.VolumeAccess.TypesTest
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
                ca <- Gen.sample genAccountSimple
                a2 <- addAccount ca
                Just auth2 <- lookupSiteAuthByEmail False (accountEmail ca)
                changeAccount (auth2 { accountPasswd = Just "somehashval"})
                changeAuthorize (makeAuthorize (Access PermissionADMIN PermissionADMIN) Nothing (accountParty a2) dbSite)
                lookupSiteAuthByEmail False (accountEmail ca))
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

-- 3 = superadmin grant edit <<<  expand upon these to ensure all inheritances are covered
-- 4 = ai grant <<<
-- 5 = ai authorize <<<

-- 6 = aff authorize

test_7 :: TestTree
test_7 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the AI creates a private volume"
    -- TODO: should be lookup auth on rootParty
    vol <- runReaderT (addVolumeSetPrivate aiAcct) aiCtxt
    step "Then the public can't view it"
    -- Implementation of getVolume PUBLIC
    mVolForAnon <- runWithNoIdent cn2 (lookupVolume ((volumeId . volumeRow) vol))
    mVolForAnon @?= Nothing

-- <<<< more cases to handle variotions of volume access and inheritance through authorization

test_8 :: TestTree
test_8 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator for some lab A and a lab B member with lab data access only"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    (aiAcct2, aiCtxt2) <- addAuthorizedInvestigatorWithInstitution' cn2
    let aiParty2 = accountParty aiAcct2
    affAcct <- addAffiliate aiCtxt2 aiParty2 PermissionNONE PermissionADMIN
    affAuth <- lookupSiteAuthNoIdent aiCtxt2 (accountEmail affAcct)
    let affCtxt = switchIdentity aiCtxt affAuth False
    step "When an AI creates a private volume for some lab A"
    -- TODO: should be lookup auth on rootParty
    let aiParty = accountParty aiAcct
    createdVol <- runReaderT (addVolumeSetPrivate aiAcct) aiCtxt
    step "Then the lab B member can't view it"
    -- Implementation of getVolume PUBLIC
    mVolForAff <- runReaderT (lookupVolume ((volumeId . volumeRow) createdVol)) affCtxt
    mVolForAff @?= Nothing

test_9 :: TestTree
test_9 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator and their affiliate with site access only"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    affAcct <- addAffiliate aiCtxt (accountParty aiAcct) PermissionREAD PermissionNONE
    affAuth <- lookupSiteAuthNoIdent aiCtxt (accountEmail affAcct)
    let affCtxt = switchIdentity aiCtxt affAuth False
    step "When an AI creates a private volume"
    -- TODO: should be lookup auth on rootParty
    vol <- runReaderT (addVolumeSetPrivate aiAcct) aiCtxt
    step "Then their lab member with site access only can't view it"
    -- Implementation of getVolume PUBLIC
    mVolForAff <- runReaderT (lookupVolume ((volumeId . volumeRow) vol)) affCtxt
    mVolForAff @?= Nothing

test_10 :: TestTree
test_10 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator for some lab A and an authorized investigator for lab B"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    (_, aiCtxt2) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the lab A AI creates a public volume"
    -- TODO: should be lookup auth on rootParty
    createdVol <- runReaderT (addVolumeWithAccess aiAcct) aiCtxt -- partially shared, but effectively same as public
    step "Then the lab B AI can't add volume acccess"
    -- Implementation of getVolume as used by postVolumeAccess
    Just volForAI2 <- runReaderT (lookupVolume ((volumeId . volumeRow) createdVol)) aiCtxt2
    volumePermission volForAI2 @?= PermissionSHARED

----- container ----
test_11 :: TestTree
test_11 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the AI creates a private volume with a fully released container"
    -- TODO: should be lookup auth on rootParty
    cid <- runReaderT
         (do
              v <- addVolumeSetPrivate aiAcct
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
              v <- addVolumeWithAccess aiAcct
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
              v <- addVolumeSetPrivate aiAcct
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
              v <- addVolumeWithAccess aiAcct
              (someMeasure, someMeasure2) <- (,) <$> Gen.sample genCreateMeasure <*> Gen.sample genCreateMeasure
              addParticipantRecordWithMeasures v [someMeasure, someMeasure2])
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
    nobodyVa <- liftIO (mkGroupVolAccess PermissionNONE Nothing nobodyParty v)
    rootVa <- liftIO (mkGroupVolAccess PermissionNONE Nothing rootParty v)
    void (changeVolumeAccess nobodyVa)
    void (changeVolumeAccess rootVa)

mkGroupVolAccess :: Permission -> Maybe Bool -> Party -> Volume -> IO VolumeAccess
mkGroupVolAccess perm mShareFull prty vol = do
    va <- Gen.sample (genGroupVolumeAccess (Just prty) vol)
    pure
        (va
             { volumeAccessIndividual = perm
             , volumeAccessChildren = perm
             , volumeAccessShareFull = mShareFull
             })

runWithNoIdent :: DBConn -> ReaderT TestContext IO a -> IO a
runWithNoIdent cn rdr = runReaderT rdr ((mkDbContext cn) { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded })

addAuthorizedInvestigatorWithInstitution' :: DBConn -> IO (Account, TestContext)
addAuthorizedInvestigatorWithInstitution' c =
    addAuthorizedInvestigatorWithInstitution c "test@databrary.org"

-- modeled after createContainer
makeAddContainer :: (MonadAudit c m) => Volume -> Maybe Release -> Maybe Day -> m (Id Container)
makeAddContainer v mRel mDate = do
    nc <- liftIO (mkContainer v mRel mDate)
    c <- addContainer nc
    pure ((containerId . containerRow) c)

-- note: modeled after create container
mkContainer :: Volume -> Maybe Release -> Maybe Day -> IO Container
mkContainer v mRel mDate = do
    c <- Gen.sample genCreateContainer
    pure
        (c { containerRelease = mRel
           , containerVolume = v
           , containerRow = (containerRow c) { containerDate = mDate }
           })

addParticipantRecordWithMeasures :: (MonadAudit c m) => Volume -> [Measure] -> m (Id Record)
addParticipantRecordWithMeasures v measures = do
    -- note: modeled after create record
    nr <- (liftIO . mkParticipantRecord) v
    r <- addRecord nr
    forM_
        measures
        (\m -> changeRecordMeasure (m { measureRecord = r }))
    pure ((recordId . recordRow) r)

-- TODO: remove from authorizetest
addVolumeWithAccess :: MonadAudit c m => Account -> m Volume
addVolumeWithAccess a = do
    v <- (liftIO . Gen.sample) genVolumeCreateSimple
    v' <- addVolume v -- note: skipping irrelevant change volume citation
    setDefaultVolumeAccessesForCreated (accountParty a) v'
    pure v'

addVolumeSetPrivate :: (MonadAudit c m) => Account -> m Volume
addVolumeSetPrivate a = do
    v <- (liftIO . Gen.sample) genVolumeCreateSimple
    v' <- addVolume v -- note: skipping irrelevant change volume citation
    setDefaultVolumeAccessesForCreated (accountParty a) v'
    setVolumePrivate v'
    pure v'

mkParticipantRecord :: Volume -> IO Record
mkParticipantRecord vol = do
    -- repeats some logic from blankRecord
    nr <- Gen.sample (genCreateRecord vol)
    pure
        (nr {
              recordRelease = Nothing
            , recordRow = (recordRow nr) { recordCategory = participantCategory }
            })
