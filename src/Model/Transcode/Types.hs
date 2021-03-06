{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Model.Transcode.Types
  ( Transcode(..)
  , TranscodePID
  , TranscodeArgs
  , transcodeAsset
  , transcodeOrig
  , transcodeId
  -- TODO: don't re-export from Transcode
  , makeTranscodeRow
  , makeTranscode
  , makeOrigTranscode
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

import Model.Kind
import Model.Id.Types
import Model.Time
import Model.Segment
import Model.Asset.Types
import Model.AssetRevision.Types
import Model.Party.Types
import Model.Permission.Types
import Model.Volume.Types

type TranscodePID = Int32
type TranscodeArgs = [String]

type instance IdType Transcode = Int32

data Transcode = Transcode
  { transcodeRevision :: !AssetRevision
  , transcodeOwner :: SiteAuth
  , transcodeSegment :: Segment
  , transcodeOptions :: TranscodeArgs
  , transcodeStart :: Maybe Timestamp
  , transcodeProcess :: Maybe TranscodePID
  , transcodeLog :: Maybe BS.ByteString
  }

transcodeAsset :: Transcode -> Asset
transcodeAsset = revisionAsset . transcodeRevision

transcodeOrig :: Transcode -> Asset
transcodeOrig = revisionOrig . transcodeRevision

transcodeId :: Transcode -> Id Transcode
transcodeId = Id . unId . assetId . assetRow . transcodeAsset

instance Kinded Transcode where
  kindOf _ = "transcode"

makeTranscodeRow :: Segment -> [Maybe String] -> Maybe Timestamp -> Maybe Int32 -> Maybe BS.ByteString -> SiteAuth -> AssetRevision -> Transcode
makeTranscodeRow s f t p l u a =
  Transcode a u s (map (fromMaybe (error "NULL transcode options")) f) t p l

makeOrigTranscode :: (AssetRevision -> Transcode) -> AssetRow -> Asset -> Transcode
makeOrigTranscode f a o = f $ AssetRevision (Asset a $ assetVolume o) o

makeTranscode :: (Asset -> Transcode) -> AssetRow -> (VolumeRolePolicy -> Volume) -> Transcode
makeTranscode t o vp = t $ Asset o $ vp RoleAdmin
