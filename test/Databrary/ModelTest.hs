{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Databrary.ModelTest where

import Control.Monad (forM_)
import Data.Time
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Has
import Databrary.Model.Audit (MonadAudit)
import Databrary.Model.Authorize
import Databrary.Model.Category
-- import Databrary.Model.Container
import Databrary.Model.Measure
import Databrary.Model.Metric
import Databrary.Model.Party
import Databrary.Model.Permission
-- import Databrary.Model.Release
import Databrary.Model.Record
-- import Databrary.Model.Slot
import Databrary.Model.Volume
import Databrary.Model.VolumeAccess
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

-- 2 = superadmin grant
-- 3 = superadmin grant edit
-- 4 = ai grant
-- 5 = ai authorize
-- 6 = aff authorize
-- 7 = ai create priv vol
-- 8 = ai lab a, lab b access
-- 9 = aff site access only
-- 10 = ai lab a, ai lab b vol access
-- 11 = public can't view priv
-- 12 = public can't see restricted release cntr
-- 13 = public can't view priv vol record

test_14 :: TestTree
test_14 = Test.stepsWithTransaction "" $ \step cn2 -> do
    step "Given an authorized investigator's created public volume with a record not attached to a container"
    ctxt <- makeSuperAdminContext cn2 "test@databrary.org"
    instParty <- addAuthorizedInstitution ctxt "New York University"
    aiAcct <- addAuthorizedInvestigator ctxt "Smith" "Raul" "raul@smith.com" instParty
    let ctxtNoIdent = ctxt { ctxIdentity = IdentityNotNeeded, ctxPartyId = Id (-1), ctxSiteAuth = view IdentityNotNeeded }
    Just aiAuth <- runReaderT (lookupSiteAuthByEmail False "raul@smith.com") ctxtNoIdent
    let aiCtxt = switchIdentity ctxt aiAuth False
    -- TODO: should be lookup auth on rootParty
    rid <- runReaderT
         (do
              v <- addVolumeWithAccess volumeExample (accountParty aiAcct)
              addParticipantRecordWithMeasures v [someBirthdateMeasure, someGenderMeasure])
         aiCtxt
    step "When the public attempts to view the record"
    -- Implementation of getRecord PUBLIC
    -- lookupRecord uses record_release func, which references any release coming from a related slot; by default there is none
    Just rcrdForAnon <- runReaderT (lookupRecord rid) ctxtNoIdent
    step "Then the public can't see the restricted measures like birthdate"
    (participantMetricBirthdate `notElem` (fmap measureMetric . getRecordMeasures) rcrdForAnon) @? "Expected birthdate to be removed"

addParticipantRecordWithMeasures :: (MonadAudit c m) => Volume -> [Record -> Measure] -> m (Id Record)
addParticipantRecordWithMeasures v mkMeasures = do
    r <- addRecord (mkParticipantRecord v)
    forM_
        mkMeasures
        (\mk -> changeRecordMeasure (mk r))
    pure ((recordId . recordRow) r)

-- TODO: remove from authorizetest
addVolumeWithAccess :: MonadAudit c m => Volume -> Party -> m Volume
addVolumeWithAccess v p = do
    v' <- addVolume v -- note: skipping irrelevant change volume citation
    setDefaultVolumeAccessesForCreated p v'
    pure v'

someBirthdateMeasure :: Record -> Measure
someBirthdateMeasure r = Measure r participantMetricBirthdate "1990-01-02"

someGenderMeasure :: Record -> Measure
someGenderMeasure r = Measure r participantMetricGender "Male"

-- TODO: remove from authorizetest
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
