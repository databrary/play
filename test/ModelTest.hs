{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module ModelTest where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
-- import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time
-- import qualified Data.Vector as V
import qualified Hedgehog.Gen as Gen
import qualified Network.HTTP.Client as HTTPC
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

import Controller.CSV (volumeCSV)
import EZID.Volume (updateEZID)
import Has
import HTTP.Client
import Model.Asset
import Model.Audit (MonadAudit)
import Model.Authorize
import Model.Category
import Model.Citation
import Model.Container
-- import Model.Container.TypesTest
import Model.Factories
import Model.Format
import Model.Identity (MonadHasIdentity)
import Model.Measure
import Model.Metric
import Model.Offset
import Model.Paginate
import Model.Party
-- import Model.Party.TypesTest
import Model.Permission
import Model.Release
import Model.Record
-- import Model.Record.TypesTest
import Model.RecordSlot
import Model.Segment
import Model.Slot
import Model.Transcode
import Model.Volume
import Model.VolumeAccess
-- import Model.VolumeAccess.TypesTest
import Service.DB (DBConn, MonadDB)
import Service.Types (Secret(..))
import Solr.Index (updateIndex)
import Solr.Search
import Solr.Service (initSolr, finiSolr)
import Store.CSV (buildCSV)
import qualified Store.Config as C
-- import Store.Asset
-- import Store.AV
import Store.Probe
import Store.Transcode
import Paths_databrary (getDataFileName)
import TestHarness as Test

test_1 :: TestTree
test_1 = Test.stepsWithTransaction "test_1" $ \step cn2 -> do
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
setIdentityNotNeeded c = c { ctxIdentity = Just IdentityNotNeeded, ctxPartyId = Just (Id (-1)) }

setSiteAuthFromIdent :: TestContext -> TestContext
setSiteAuthFromIdent c = c { ctxSiteAuth = Just (view ((fromJust . ctxIdentity) c)) }

setDefaultRequest :: TestContext -> TestContext
setDefaultRequest c = c { ctxRequest = Just defaultRequest }

----- site authorize granting -----
test_2 :: TestTree
test_2 = Test.stepsWithTransaction "test_2" $ \step cn -> do
    step "Given a superadmin"
    let adminEmail = "test@databrary.org"
    ctxt <- makeSuperAdminContext cn adminEmail
    step "When the superadmin grants the institution admin access on the db site"
    instParty <- addAuthorizedInstitution ctxt
    authorization1 <- runReaderT (lookupAuthorization instParty rootParty) ctxt
    step "Then we expect the institution to have ADMIN site access, no member privileges"
    authorizeAccess authorization1 @?= Access { accessSite' = PermissionADMIN, accessMember' = PermissionNONE }

-- TODO:  expand upon these to ensure all inheritances are covered
test_3 :: TestTree
test_3 = Test.stepsWithTransaction "test_3" $ \step cn2 -> do
    -- Note to self: beyond documentation, this a long winded way of testing authorize_view
    step "Given a superadmin and an institution authorized as admin under db site"
    step "When the superadmin grants an authorized investigator with edit access on their parent institution"
    (_, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "Then we expect the authorized investigator to effectively have edit db site access"
    siteAccess ((fromJust . ctxSiteAuth) aiCtxt) @?= Access { accessSite' = PermissionEDIT, accessMember' = PermissionNONE }

test_4 :: TestTree
test_4 = Test.stepsWithTransaction "test_4" $ \step cn2 -> do
    step "Given an authorized investigator"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    let aiParty = accountParty aiAcct
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

test_5 :: TestTree
test_5 = Test.stepsWithTransaction "test_5" $ \step cn2 -> do
    step "Given an authorized investigator"
    (_, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the AI attempts to authorize some party as a superadmin on db site"
    Just p <- runReaderT (lookupAuthParty ((partyId . partyRow) rootParty)) aiCtxt
    step "Then the attempt fails during the check for privileges on db site party"
    -- guts of checkPermission, as used by getParty and postAuthorize - <= ADMIN
    partyPermission p @?= PermissionSHARED

test_6 :: TestTree
test_6 = Test.stepsWithTransaction "test_6" $ \step cn2 -> do
    step "Given an affiliate (with high priviliges)"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    affAcct <- addAffiliate aiCtxt (accountParty aiAcct) PermissionREAD PermissionADMIN
    gradAffAuth <- lookupSiteAuthNoIdent aiCtxt (accountEmail affAcct)
    let affCtxt = switchIdentity aiCtxt gradAffAuth False
    step "When affiliate attempts to authorize anybody to any other party"
    Just _ <- runReaderT (lookupAuthParty ((partyId . partyRow . accountParty) affAcct)) affCtxt
    step "Then the attempt fails during the check for privileges on the parent party"
    -- guts of checkPermission, as used by getParty and postAuthorize - <= ADMIN
    -- FAILING - needs change in postAuthorize
    -- partyPermission p @?= PermissionEDIT

------ volume --------
test_7 :: TestTree
test_7 = Test.stepsWithTransaction "test_7" $ \step cn2 -> do
    step "Given an authorized investigator"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the AI creates a private volume"
    -- TODO: should be lookup auth on rootParty
    vol <- runReaderT (addVolumeSetPrivate aiAcct) aiCtxt
    step "Then the public can't view it"
    mVol <-
        runWithNoIdent
            cn2
            (getVolume PermissionREAD ((volumeId . volumeRow) vol))
    mVol @?= LookupFailed

volWithId :: Volume -> (Id Volume, Volume)
volWithId v = (volumeId (volumeRow v), v)

test_7_accessOwnVolume :: TestTree
test_7_accessOwnVolume = Test.stepsWithTransaction "can access own volume" $ \step cn2 -> do
    step "Given an authorized investigator"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the AI creates a private volume"
    -- TODO: should be lookup auth on rootParty
    (volId, _) <- volWithId <$> runReaderT (addVolumeSetPrivate aiAcct) aiCtxt
    step "Then the AI can view it"
    mVol <- runReaderT (getVolume PermissionREAD volId) aiCtxt
    case mVol of
        -- FIXME: vol' is not equal to vol.
        LookupFound (volWithId -> (volId', _)) -> volId' @?= volId
        LookupFailed -> assertFailure "Lookup failed"
        LookupDenied -> assertFailure "Lookup denied"

-- <<<< more cases to handle variations of volume access and inheritance through authorization

test_8 :: TestTree
test_8 = Test.stepsWithTransaction "test_8" $ \step cn2 -> do
    step "Given an authorized investigator for some lab A and a lab B member with lab data access only"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    (aiAcct2, aiCtxt2) <- addAuthorizedInvestigatorWithInstitution' cn2
    let aiParty2 = accountParty aiAcct2
    affAcct <- addAffiliate aiCtxt2 aiParty2 PermissionNONE PermissionADMIN
    affAuth <- lookupSiteAuthNoIdent aiCtxt2 (accountEmail affAcct)
    let affCtxt = switchIdentity aiCtxt affAuth False
    step "When an AI creates a private volume for some lab A"
    -- TODO: should be lookup auth on rootParty
    createdVol <- runReaderT (addVolumeSetPrivate aiAcct) aiCtxt
    step "Then the lab B member can't view it"
    mVol <-
        runReaderT
            (getVolume PermissionPUBLIC ((volumeId . volumeRow) createdVol))
            affCtxt
    mVol @?= LookupFailed

test_9 :: TestTree
test_9 = Test.stepsWithTransaction "test_9" $ \step cn2 -> do
    step "Given an authorized investigator and their affiliate with site access only"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    affAcct <- addAffiliate aiCtxt (accountParty aiAcct) PermissionREAD PermissionNONE
    affAuth <- lookupSiteAuthNoIdent aiCtxt (accountEmail affAcct)
    let affCtxt = switchIdentity aiCtxt affAuth False
    step "When an AI creates a private volume"
    -- TODO: should be lookup auth on rootParty
    vol <- runReaderT (addVolumeSetPrivate aiAcct) aiCtxt
    step "Then their lab member with site access only can't view it"
    mVol <-
        runReaderT
            (getVolume PermissionPUBLIC ((volumeId . volumeRow) vol))
            affCtxt
    mVol @?= LookupFailed

test_10 :: TestTree
test_10 = Test.stepsWithTransaction "test_10" $ \step cn2 -> do
    step "Given an authorized investigator for some lab A and an authorized investigator for lab B"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    (_, aiCtxt2) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the lab A AI creates a public volume"
    -- TODO: should be lookup auth on rootParty
    createdVol <- runReaderT (addVolumeWithAccess aiAcct) aiCtxt -- partially shared, but effectively same as public
    step "Then the lab B AI can't add volume acccess"
    -- FIXME: Hard to express properly with the confusion between permissions
    -- and roles.
    Just volForAI2 <- runReaderT (lookupVolume ((volumeId . volumeRow) createdVol)) aiCtxt2
    volumeRolePolicy volForAI2 @?= RoleSharedViewer SharedRestrictedPolicy

test_10_1 :: TestTree
test_10_1 = Test.stepsWithTransaction "Denied elevated access" $ \step cn2 -> do
    step "Given an authorized investigator for some lab A and an authorized investigator for lab B"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    (_, aiCtxt2) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the lab A AI creates a public volume"
    -- TODO: should be lookup auth on rootParty
    -- NB: partially shared, but effectively same as public
    (volId, _) <- volWithId <$> runReaderT (addVolumeWithAccess aiAcct) aiCtxt
    step "Then the lab B AI can't access it with edit privileges"
    mVol <- runReaderT (getVolume PermissionEDIT volId) aiCtxt2
    mVol @?= LookupDenied

----- container ----
test_11 :: TestTree
test_11 = Test.stepsWithTransaction "test_11" $ \step cn2 -> do
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
test_12 = Test.stepsWithTransaction "test_12" $ \step cn2 -> do
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

----- asset ---------
{-
_test_storage :: TestTree
_test_storage = Test.stepsWithTransaction "" $ \step cn2 -> do
    (_, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    aiCtxt2 <- withStorage aiCtxt
    res <- runReaderT
        (do
              Just asset <- lookupAsset (Id 1)
              getAssetFile asset)
        aiCtxt2
    res @?= Just "./store/3d/da3931202cbe06a9e4bbb5f0873c879121ef0a"
-}

test_12a :: TestTree
test_12a = ignoreTest $ -- "Invalid cross-device link"
  Test.stepsWithTransaction "test_12a" $ \step cn2 -> do
    step "Given a partially shared volume"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    -- TODO: should be lookup auth on rootParty
    vol <- runReaderT (addVolumeSetPublic aiAcct) aiCtxt
    step "When a publicly released asset is added"
    aiCtxt2 <- withStorage aiCtxt
    foundAsset <- runReaderT
        (do
              (a, contents) <- Gen.sample (genCreateAssetAfterUpload vol)
              let a' = a { assetRow = (assetRow a) { assetRelease = Just ReleasePUBLIC } }
              let path = "/tmp/test1.csv" -- save contents to tmpdir
              liftIO (BS.writeFile path contents)
              let rpath = "/tmp/test1.csv" -- TODO: convert from filepath to rawfilepath
              savedAsset <- addAsset a' (Just rpath)
              pure savedAsset
              -- change asset
              -- gen assetslot
              -- change assetslot
              -- lookup assetslot or container w/contents
              )
        aiCtxt2
    -- Just slotForAnon <- runWithNoIdent cn2 (lookupSlotByContainerId cid)
    step "Then one can retrieve the asset"
    (assetRelease . assetRow) foundAsset @?= Just ReleasePUBLIC
    {-
    -- get assetsegment
    --  -- > check asset name, format, contents, release, size, duration, slot segment, container size
    -}
    -- TODO: bracket with delete file

-- same as above, but for video file that undergoes conversion
test_12b :: TestTree
-- FIXME: ignored because it hangs indefinitely for bryan
test_12b = ignoreTest $ localOption NoTimeout $ Test.stepsWithResourceAndTransaction "test_12b" $ \step ist cn2 -> do
    step "Given a partially shared volume"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    -- TODO: should be lookup auth on rootParty
    vol <- runReaderT (addVolumeSetPublic aiAcct) aiCtxt
    step "When a publicly released video asset is added"
    aiCtxt2 <- withStorage aiCtxt
    aiCtxt3 <- withAV aiCtxt2
    let aiCtxt4 = withTimestamp (UTCTime (fromGregorian 2017 5 6) (secondsToDiffTime 0)) aiCtxt3
    aiCtxt5 <- withLogs aiCtxt4
    let aiCtxt6 = withInternalStateVal ist aiCtxt5
    foundAsset <- runReaderT
        (do
              (a, _) <- Gen.sample (genCreateAssetAfterUpload vol)
              let a' = a { assetRow = (assetRow a) { assetRelease = Just ReleasePUBLIC, assetFormat = (fromJust . getFormatByExtension) "webm" } }
              rpath <- liftIO (BSC.pack <$> getDataFileName "test/data/small.webm") -- TODO: touch timestamp to trigger new file
              Right probe <- probeFile "small.webm" rpath
              savedAsset <- addAsset a' (Just rpath)
              let assetWithName = savedAsset { assetRow = (assetRow savedAsset) { assetName = Just "small.webm" } }
              -- gen assetslot
              -- change assetslot
              trans <- addTranscode assetWithName fullSegment defaultTranscodeOptions probe
              _ <- startTranscode trans
              Just t <- lookupTranscode (transcodeId trans)
              if (assetSHA1 . assetRow . transcodeAsset) t /= Nothing -- there was an existing matching transcode
              then
                 (pure (transcodeAsset t))
              else do
                  -- TODO (needs asset slot created above); needs sha1; how deal with determining exit code?
                  () <- liftIO (detectJobDone (transcodeId trans))
                  collectTranscode t 0 Nothing ""
                  Just t2 <- lookupTranscode (transcodeId trans)
                  pure (transcodeAsset t2))
                  -- lookup assetslot or container w/contents
        (aiCtxt6 { ctxSecret = Just (Secret "abc"), ctxRequest = Just mkRequest }) -- use mkRequest to override default domain
    step "Then one can retrieve the resulting asset"
    (assetRelease . assetRow) foundAsset @?= Just ReleasePUBLIC
    (assetName . assetRow) foundAsset @?= Just "small.webm"
    (assetDuration . assetRow) foundAsset @?= Just (Offset 5.62)

-- | Can either detect event in log, look for completed output, or wait for process to stop.
-- Currently, detect event in log.
detectJobDone :: Id Transcode -> IO ()
detectJobDone transId@(Id idVal) = do
    (exit, _, _) <- readProcessWithExitCode "grep" ["curl", "trans/" ++ show idVal ++ ".log"] ""
    case exit of
        ExitSuccess -> pure ()
        ExitFailure _ -> threadDelay 500000 >> detectJobDone transId

----- record ---
test_13 :: TestTree
test_13 = Test.stepsWithTransaction "test_13" $ \step cn2 -> do
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
test_14 = Test.stepsWithTransaction "test_14" $ \step cn2 -> do
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

------ miscellaneous -----
test_15 :: TestTree
test_15 = Test.stepsWithTransaction "test_15" $ \step cn2 -> do
    step "Given a public volume with one container"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    -- TODO: should be lookup auth on rootParty
    (vol, cntr) <- runReaderT
         (do
              v <- addVolumeSetPublic aiAcct
              c <- makeAddContainer v (Just ReleasePUBLIC) Nothing
              Just v' <- lookupVolume ((volumeId . volumeRow) v)
              Just s <- lookupSlotByContainerId c
              pure (v', slotContainer s))
         aiCtxt
    step "When the public downloads the volume as a zip"
    -- Implementation of zipVolume
    -- TODO: expand to test zipVolume up to zipActs
    csv <- runWithNoIdent cn2
        (do
            _:cr <- lookupVolumeContainersRecords vol
            rows <- volumeCSV vol cr
            pure (buildCSV rows))
    step "Then zip contains a CSV summarizing the volume"
    BSB.toLazyByteString csv
      @?=
        ("session-id,session-name,session-date,session-release\n"
         <> (BSLC.pack . show . containerId . containerRow) cntr <> ","
          <> (BSLC.pack . Data.Maybe.maybe "" T.unpack . containerName . containerRow) cntr <> ","
          <> ",\n")

test_15b :: TestTree
test_15b = Test.stepsWithTransaction "test_15b" $ \step cn2 -> do
    step "Given a created volume"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    -- TODO: should be lookup auth on rootParty
    v <- runReaderT (addVolumeWithAccess aiAcct) aiCtxt
    step "When a link is added to the volume"
    link <- Gen.sample genVolumeLink
    runReaderT (changeVolumeLinks v [link]) aiCtxt
    step "Then one can view the link"
    volLinks <- runWithNoIdent cn2 (lookupVolumeLinks v)
    volLinks @?= [link]

------- search -------------
test_16 :: TestTree
test_16 = ignoreTest $ -- TODO: enable this inside of nix build with solr binaries and core installed in precheck
    Test.stepsWithTransaction "test_16" $ \step cn2 -> do
        step "Given an authorized investigator"
        (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
        step "When the AI creates a partially shared volume and the search index is updated"
        -- TODO: should be lookup auth on rootParty
        vol <- runReaderT (addVolumeWithAccess aiAcct) aiCtxt
        conf <- C.load "databrary.conf"
        solr <- initSolr True (conf C.! "solr")
        threadDelay 2000000 -- TODO: poll until solr is up
        hc <- initHTTPClient
        step "Then the public will find a pre-existing volume by title"
        lbs <- runReaderT (search (mkVolumeSearchQuery "databrary")) (aiCtxt { ctxSolr = Just solr, ctxHttpClient = Just hc})
        let Just resVal = decode (HTTPC.responseBody lbs) :: Maybe Value
        show resVal @?= -- TODO: use aeson parser and only check selected fields
            "Object (fromList [(\"response\",Object (fromList [(\"numFound\",Number 1.0),(\"start\",Number 0.0),(\"docs\",Array [Object (fromList [(\"body\",String \"Databrary is an open data library for developmental science. Share video, audio, and related metadata. Discover more, faster.\\nMost developmental scientists rely on video recordings to capture the complexity and richness of behavior. However, researchers rarely share video data, and this has impeded scientific progress. By creating the cyber-infrastructure and community to enable open video sharing, the Databrary project aims to facilitate deeper, richer, and broader understanding of behavior.\\nThe Databrary project is dedicated to transforming the culture of developmental science by building a community of researchers committed to open video data sharing, training a new generation of developmental scientists and empowering them with an unprecedented set of tools for discovery, and raising the profile of behavioral science by bolstering interest in and support for scientific research among the general public.\"),(\"name\",String \"Databrary\"),(\"owner_names\",Array [String \"Admin, Admin\",String \"Steiger, Lisa\",String \"Tesla, Testarosa\"]),(\"id\",Number 1.0),(\"owner_ids\",Array [Number 1.0,Number 3.0,Number 7.0])])])])),(\"spellcheck\",Object (fromList [(\"suggestions\",Array [])]))])"
        step "and the public will find the newly added volume by title"
        bctx <- mkSolrIndexingContextSimple cn2 solr
        runReaderT updateIndex bctx
        _ <- runReaderT (search (mkVolumeSearchQuery ((volumeName . volumeRow) vol))) (aiCtxt { ctxSolr = Just solr, ctxHttpClient = Just hc})
        -- TODO: assert one result, and result name matches volume title searched for
        -- TODO: how reset index after a test? reindex after transaction is rolled back for now...
        finiSolr solr

-------- ezid --------------
test_17 :: TestTree
test_17 = localOption (mkTimeout (10 * 10^(6 :: Int))) $ Test.stepsWithResourceAndTransaction "test_17" $ \step ist cn2 -> do
    step "Given an authorized investigator"
    (aiAcct, aiCtxt) <- addAuthorizedInvestigatorWithInstitution' cn2
    step "When the AI creates a partially shared volume"
    step "and add data that will be indexed with ezid"
    step "and the ezid generation runs"
    -- TODO: should be lookup auth on rootParty
    vol <- runReaderT
        (do
            v <- addVolumeWithAccess aiAcct
            l <- liftIO (Gen.sample genVolumeLink)
            changeVolumeLinks v [l]
            pure v
        )
        aiCtxt
    -- updateEZID
    -- type EZIDM a = CookiesT (ReaderT EZIDContext IO) a
    bctx <- mkBackgroundContext ForEzid ist cn2
    mEzidWasUp <- runReaderT updateEZID bctx
    step "Then the volume will have a valid doi" -- TODO; and ezid will expose registered info somehow?
    mEzidWasUp @?= Just True  -- Nothing = ezid not initialized; Just False = initalized, but down
    Just _vol' <- runReaderT (lookupVolume ((volumeId . volumeRow) vol)) aiCtxt
    -- let Just doi = (volumeDOI . volumeRow) vol'
    -- TODO: check doi link causes a redirect to Location: http://databrary.org/volume/801 with the volume id matching above
    --   example - https://doi.org/10.5072/FK2.801
    pure ()

-- remaining complex subsystems to demonstrate using: notifications, upload, ingest

------------------------------------------------------ end of tests ---------------------------------

mkVolumeSearchQuery :: T.Text -> SearchQuery
mkVolumeSearchQuery searchStr =
    SearchQuery {
          searchString = Just searchStr
        , searchFields = []
        , searchMetrics = []
        , searchType = SearchVolumes
        , searchPaginate = Paginate { paginateOffset = 0, paginateLimit = 12 }
        }

accessIsEq :: Access -> Permission -> Permission -> Assertion
accessIsEq a site member = a @?= Access { accessSite' = site, accessMember' = member }

lookupSlotByContainerId :: (MonadDB c m, MonadHasIdentity c m) => Id Container -> m (Maybe Slot)
lookupSlotByContainerId cid = lookupSlot (containerSlotId cid)

setVolumePrivate :: (MonadAudit c m) => Volume -> m ()
setVolumePrivate v = do
    nobodyVa <- liftIO (mkGroupVolAccess PermissionNONE Nothing nobodyParty v)
    rootVa <- liftIO (mkGroupVolAccess PermissionNONE Nothing rootParty v)
    void (changeVolumeAccess nobodyVa)
    void (changeVolumeAccess rootVa)

setVolumePublic :: (MonadAudit c m) => Volume -> m ()
setVolumePublic v = do
    nobodyVa <- liftIO (mkGroupVolAccess PermissionPUBLIC (Just True) nobodyParty v)
    rootVa <- liftIO (mkGroupVolAccess PermissionSHARED (Just True) rootParty v)
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
runWithNoIdent cn rdr =
    runReaderT
        rdr
        ((mkDbContext cn) { ctxIdentity = Just IdentityNotNeeded, ctxPartyId = Just (Id (-1)), ctxSiteAuth = Just (view IdentityNotNeeded) })

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

addVolumeSetPublic :: (MonadAudit c m) => Account -> m Volume
addVolumeSetPublic a = do
    v <- (liftIO . Gen.sample) genVolumeCreateSimple
    v' <- addVolume v -- note: skipping irrelevant change volume citation
    setDefaultVolumeAccessesForCreated (accountParty a) v'
    setVolumePublic v'
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
