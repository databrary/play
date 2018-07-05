{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Factories where

import qualified Data.ByteString as BS
import Data.Maybe
import Data.Fixed
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.URI
-- import Test.Tasty.HUnit

import Model.Age
import Model.Asset
import Model.Category
import Model.Citation.Types
import Model.Container
import Model.Format
import Model.GeoNames
import Model.Id
-- import Model.Identity.Types
import Model.Metric
import Model.Offset
import Model.Party.Types
import Model.Permission
import Model.Record.Types
-- import Model.Release.Types
import Model.Slot.Types
import Model.Time
-- import Model.Transcode
import Model.TypeOrphans ()
-- import Model.Slot.Types
import Model.Volume.Types
import Model.VolumeAccess.Types

----- general utilities ------
genDate :: Gen Date
genDate =
      fromGregorian
          <$> Gen.integral (Range.constant 1990 2015)
          <*> Gen.integral (Range.constant 1 12)
          <*> Gen.integral (Range.constant 1 28)
{-
genVolumeCreationTime :: Gen UTCTime
genVolumeCreationTime =
    UTCTime
          <$> (fromGregorian
                   <$> Gen.integral (Range.constant 2000 2018)
                   <*> Gen.integral (Range.constant 1 12)
                   <*> Gen.integral (Range.constant 1 28))
          <*> (secondsToDiffTime <$> Gen.integral (Range.constant 0 86399))
-}

genGeneralURI :: Gen URI
genGeneralURI = do
    domain <- Gen.string (Range.constant 1 20) Gen.alphaNum
    pathSeg1 <- Gen.string (Range.constant 1 20) Gen.alphaNum -- TODO: generate multiple segs, allowed chars?
    scheme1 <- Gen.element ["http:", "https:"]
    pure
        (nullURI {
              uriScheme = scheme1
            , uriAuthority = Just (URIAuth "" ("www." <> domain <> ".com") "") -- TODO: choose on prefix and suffix?
            , uriPath = "/" <> pathSeg1
            })

----- value objects ------

-- id -- usually have db generate db

-- maskeddate
genMaskedDate :: Gen MaskedDate
genMaskedDate = do
    dt <- genDate
    mask <- Gen.bool
    pure (maskDateIf mask dt)

-- geoname
genGeoName :: Gen GeoName
genGeoName = do
    -- TODO: better generator, longer list from a csv file?
    (i, nm) <- Gen.element [(3041565, "Andorra"), (3351879, "Angola")]
    pure (GeoName (Id i) nm)

-- orcid

-- url
----- gen doi value
----- gen hdl value
----- gen doi url
----- gen hdl url

-- release

-- offset
genOffset :: Milli -> Gen Offset
genOffset totalLength =
    Offset <$> Gen.realFrac_ (Range.constant 0 totalLength)

-- segment

-- age
genAge :: Gen Age
genAge =
  let maxAgeTypicallyStudied = 14
  in Age <$> Gen.integral (Range.constant 0 (maxAgeTypicallyStudied*365))

-- format
genFormat :: Gen Format
genFormat = Gen.element allFormats

genAVFormat :: Gen Format
genAVFormat = Gen.element (filter formatIsAV allFormats)

genNotAVFormat :: Gen Format
genNotAVFormat = Gen.element (filter formatNotAV allFormats)

genTranscodeOutputFormat :: Gen Format
genTranscodeOutputFormat = Gen.element (catMaybes (fmap formatTranscodable allFormats))

-- category
genCategory :: Gen Category
genCategory = Gen.element allCategories

-- ...

-- funder

-- tag

----- entities ------ 

-- party
genPartyId :: Gen (Id Party)
genPartyId = Id <$> Gen.integral (Range.constant 3 5000)

genPartySortName :: Gen Text
genPartySortName = Gen.text (Range.constant 0 80) Gen.alpha

genPartyPreName :: Gen Text
genPartyPreName = Gen.text (Range.constant 0 80) Gen.alpha

genPartyAffiliation :: Gen Text
genPartyAffiliation = Gen.text (Range.constant 0 150) Gen.alpha

genPartyRowSimple :: Gen PartyRow
genPartyRowSimple =
    PartyRow
        <$> genPartyId
        <*> genPartySortName
        <*> Gen.maybe genPartyPreName
        <*> pure Nothing
        <*> Gen.maybe genPartyAffiliation
        <*> pure Nothing
-- TODO: split into group, ai, collaborator, lab manager, lab staff

genInstitutionUrl :: Gen (Maybe URI)
genInstitutionUrl =
    Just <$> pure ((fromJust . parseURI) "https://www.nyu.edu")

genAccountEmail :: Gen BS.ByteString
genAccountEmail = pure "adam.smith@nyu.edu"

genPartySimple :: Gen Party
genPartySimple = do
   let gPerm = pure PermissionPUBLIC
   let gAccess = pure Nothing
   p <- Party <$> genPartyRowSimple <*> pure Nothing <*> pure Nothing <*> gPerm <*> gAccess
   a <- Account <$> genAccountEmail <*> pure p
   (let p2 = p { partyAccount = Just a2 } -- account expected below
        a2 = a { accountParty = p2 }
    in pure p2)

-- TODO: get rid of this and genPartySimplex
genAccountSimple :: Gen Account
genAccountSimple = do
    firstName <- genPartyPreName
    lastName <- genPartySortName
    email <- (\d -> TE.encodeUtf8 (firstName <> "." <> lastName <> "@" <> d)) <$> Gen.element ["nyu.edu", "wm.edu"]
    (let pr = (partyRow blankParty) { partySortName = lastName , partyPreName = Just firstName }
         p = blankParty { partyRow = pr, partyAccount = Just a }
         a = blankAccount { accountParty = p, accountEmail = email }
     in pure a)

genCreateInstitutionParty :: Gen Party
genCreateInstitutionParty = do
   let bp = blankParty
   url <- genInstitutionUrl
   mPreName <- Gen.maybe (pure "The")
   sortName <- genPartySortName
   pure
       (bp {
             partyRow = (partyRow bp) {
                 partySortName = sortName
               , partyPreName = mPreName
               , partyURL = url
               }
           })
{-
-- identity
genInitialIdentNeedAuthRoutes :: SiteAuth -> Gen Identity
genInitialIdentNeedAuthRoutes =
    Gen.choice
        [ pure NotLoggedIn
        , Identified <$> undefined -- TODO: finish gen session in Token types
        ]

genInitialIdentOpenRoutes :: Gen Identity
genInitialIdentOpenRoutes =
    pure IdentityNotNeeded

genReIdentified :: SiteAuth -> Gen Identity
genReIdentified =
    ReIdentified <$> genSiteAuthSimple -- TODO: come up with a better site auth generator
-}

-- token

-- authorize
---- genCreateAuthorizeReq :: Party -> Party -> Gen Authorize




-- volume / citation
genVolumeName :: Gen Text  -- Verify this and next two with real data profile
genVolumeName = Gen.text (Range.constant 0 200) Gen.alphaNum

genVolumeBody :: Gen Text
genVolumeBody = Gen.text (Range.constant 0 300) Gen.alphaNum

genVolumeAlias :: Gen Text
genVolumeAlias = Gen.text (Range.constant 0 60) Gen.alphaNum

{-
genVolumeDOI :: Gen BS.ByteString
genVolumeDOI = pure "10.17910/B7159Q" -- TODO: good generator for this?
-}

-- Note: keep this in sync with changes in Controller.createVolume
genVolumeCreateSimple :: Gen Volume
genVolumeCreateSimple = do
    let bv = blankVolume
    name <- genVolumeName
    mBody <- Gen.maybe genVolumeBody
    mAlias <- Gen.maybe genVolumeAlias
    pure
      (bv {
        volumeRow = (volumeRow bv) {
              volumeName = name
            , volumeBody = mBody
            , volumeAlias = mAlias
            }
       })

-- vol acc
genGroupPermission :: Party -> Gen (Permission, Maybe Bool)
genGroupPermission p
  | partyRow p == partyRow rootParty =
        Gen.element [(PermissionNONE, Nothing), (PermissionSHARED, Just False), (PermissionSHARED, Just True)]
  | partyRow p == partyRow nobodyParty =
        Gen.element [(PermissionNONE, Nothing), (PermissionPUBLIC, Just False), (PermissionPUBLIC, Just True)]
  | otherwise = error "only known group parties that should get volume access are root party and nobody party"

genGroupVolumeAccess :: Maybe Party -> Volume -> Gen VolumeAccess
genGroupVolumeAccess mGroup vol = do
    group <- maybe (Gen.element [nobodyParty, rootParty]) pure mGroup
    (perm, mShareFull) <- genGroupPermission group
    VolumeAccess
       <$> pure perm
       <*> pure perm
       <*> Gen.maybe (Gen.integral (Range.constant 1 20)) -- TODO: what does this field mean?
       <*> pure mShareFull
       <*> pure group
       <*> pure vol



-- container / slot
genContainerTestDay :: Gen Day
genContainerTestDay =
    fromGregorian
                 <$> Gen.integral (Range.constant 2000 2018)
                 <*> Gen.integral (Range.constant 1 12)
                 <*> Gen.integral (Range.constant 1 28)

genContainerName :: Gen Text
genContainerName = Gen.text (Range.constant 0 80) Gen.alphaNum

genCreateContainerRow :: Gen ContainerRow
genCreateContainerRow =
   -- some redundancy with blankContainer
    ContainerRow
        <$> (pure . error) "container id created after save"
        <*> pure False -- Gen.bool -- TODO: when is top valid?
        <*> Gen.maybe genContainerName
        <*> Gen.maybe genContainerTestDay

genCreateContainer :: Gen Container
genCreateContainer =
    -- some redundancy with blankContainer
    Container
        <$> genCreateContainerRow
        <*> Gen.maybe Gen.enumBounded
        <*> (pure . error) "container volume not specified"

-- upload / asset / assetslot / assetsegment / assetrevision

genUploadFileName :: Format -> Gen Text
genUploadFileName fmt = do
    let ext = (TE.decodeUtf8 . head . formatExtension) fmt
    prefix <- Gen.text (Range.constant 0 80) Gen.alphaNum -- include spaces?
    pure (prefix <> "." <> ext)

---- genCreateUpload :: Volume -> Gen Upload
---- genSendFileChunk :: File -> Gen Chunk
---- genFileContents :: Format -> Gen BS.ByteString

genCreateAssetAfterUpload :: Volume -> Gen (Asset, BS.ByteString)
genCreateAssetAfterUpload vol = do -- modeled after processAsset (AssetTargetVolume ..) w/name,container,upload,maybe release
    -- TODO: who should create the asset file?
    let ba = blankAsset vol
    fmt <- pure (getFormat' (Id 2)) -- csv; TODO: general format + file contents
    mName <- Just <$> genUploadFileName fmt
    mRel <- Gen.maybe Gen.enumBounded
    contents <- pure "col1,col2\n123,456\n"
    -- duration, sha1, size remain nothing from blankAsset
    pure
        (ba {
             assetRow = (assetRow ba) {
                  assetFormat = fmt
                , assetRelease = mRel
                , assetName = mName
                }
            }
        , contents)

-- TODO: this assumes that the asset has been updated with real name
genCreateSlotAssetAfterUpload :: Asset -> Slot -> Gen Asset
genCreateSlotAssetAfterUpload _ _ = do  -- modeled after processAsset (AssetTargetVolume ..) w/name,container,upload, maybe release
    -- assetNoSlot (blankAsset v)
    -- lookupVolContainer -> slotContainer -> build up segment into slot
    -- fix asset slot duration
    --   assetName = read name
    -- 
    pure undefined

-- mkTranscodeFromInputAsset :: Asset -> a -> Transcode
-- mkTranscodeFromInputAsset orig probe =
--     undefined
-- transcode
---- genCreateTranscode :: Asset -> ...
-- probe <- fileuploadprobe upfile
-- trans <- model.add transcode
--    (asset with duration, no sha1 and no size; name is upload name; has rel and fmt)
--    fullSegment
--    defaultTranscodeOptions
--    probe
-- starttranscode trans

-- the script eventually uses postback to remoteTranscode (during tests, ignore postback, but simulate its actions)
--    lookupTranscode
--    collectTranscode with submitted exit code, sha1, logs (need to recreate sha1 using sha1sum command)
--      updateTranscode
--      maketempfile
--      ctlTranscode w/tempfile
--      updatetranscode
--      avprobe tempfile
--      a <- changeAsset w/sha1, (dur probe), tempfile
--      changeAssetSlotDuration a


-- excerpt
---- genCreateExcerpt :: Asset -> Gen Excerpt



-- measure / metric
----- TODO: expand these to really generate random measure values
genBirthdateMeasure :: Gen (Metric, BS.ByteString)
genBirthdateMeasure =
    pure (participantMetricBirthdate, "1990-01-02")

genGenderMeasure :: Gen (Metric, BS.ByteString)
genGenderMeasure =
    pure (participantMetricGender, "Male")

genParticipantMetricValue :: Gen (Metric, BS.ByteString)
genParticipantMetricValue =
    Gen.choice [genBirthdateMeasure, genGenderMeasure]

genCreateMeasure :: Gen Measure
genCreateMeasure = do
    (mtrc, val) <- genParticipantMetricValue
    Measure
        <$> (pure . error) "measure record not set yet"
        <*> pure mtrc
        <*> pure val
-- record
genCreateRecord :: Volume -> Gen Record
genCreateRecord vol = do
    -- repeats some logic from blankRecord
    Record
        <$> (RecordRow <$> (pure . error) "Id set after saved" <*> genCategory)
        <*> pure []
        <*> Gen.maybe Gen.enumBounded
        <*> pure vol
----- genAddVolumeCategory
----- genAddVolumeMetric
-- recordslot
----- genCreateRecordSlot :: Slot -> Record -> Gen RecordSlot



-- funding
----- genCreateVolumeFunding :: Gen Funding
-- links
genVolumeLink :: Gen Citation
genVolumeLink =
    -- TODO: use real generators below
    -- TODO: some repetition from postVolumeLinks form parsing, create blankLink function
    -- TODO: pass the link name into the URI generator
    Citation
        <$> Gen.text (Range.constant 0 50) Gen.alpha
        <*> (Just <$> genGeneralURI)
        <*> pure Nothing
        <*> pure Nothing


-- notification
-- audit
-- ingest
-- vol state
---- genCreateVolumeState :: Volume -> Gen VolumeState
----    generate key value pair and is public
{-
volumeState1 :: VolumeState
volumeState1 =
  VolumeState {
        volumeStateKey = "key1"
      , volumeStateValue = Null
      , volumeStatePublic = True
      , stateVolume = vol
      }
-}
-- activity
-- stats
-- comment
----- genCreateComment :: Slot -> Maybe Parent -> Comment
-- tag use
----- genCreateTagUse
----- genCreateKeywordUse
