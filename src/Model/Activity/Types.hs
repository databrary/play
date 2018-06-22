module Model.Activity.Types
  ( ActivityTarget(..)
  , Activity(..)
  ) where

import qualified Data.ByteString as BS

import Model.Audit.Types
import Model.Id.Types
import Model.Release.Types
import Model.Party.Types
import Model.Authorize.Types
import Model.Volume.Types
import Model.VolumeAccess.Types
import Model.Container.Types
import Model.Segment
import Model.Slot.Types
-- import Model.Record.Types
-- import Model.RecordSlot.Types
import Model.Asset.Types

data ActivityTarget
  = ActivityParty         { activityPartyRow :: !PartyRow }
  | ActivityAccount       { activityAccountId :: !(Id Party)
                          , activityAccountEmail :: !BS.ByteString
                          , activityAccountPassword :: !(Maybe BS.ByteString)
                          }
  | ActivityAuthorize     { activityAuthorize :: !Authorize }
  | ActivityVolume        { activityVolumeRow :: !VolumeRow }
  | ActivityAccess        { activityAccess :: !VolumeAccess }
  | ActivityContainer     { activityContainer :: !ContainerRow }
  | ActivityRelease       { activitySlotId :: !SlotId
                          , activityRelease :: !Release
                          }
  -- | ActivityRecord     { activityRecordRow :: !RecordRow }
  -- | ActivityRecordSlot { activityRecordSlot :: !RecordSlot }
  | ActivityAsset         { activityAssetRow :: !AssetRow }
  | ActivityAssetSlot     { activityAssetId :: !(Id Asset)
                          , activitySlotId :: !SlotId }
  | ActivityAssetAndSlot  { activityAssetRow :: !AssetRow
                          , activitySlotId :: !SlotId }
  | ActivityExcerpt       { activityAssetId :: !(Id Asset)
                          , activitySegment :: !Segment
                          , activityExcerptRelease :: !(Maybe Release)
                          }

data Activity = Activity
  { activityAudit :: !Audit
  , activityTarget :: !ActivityTarget
  , activityPrev :: Maybe ActivityTarget
  , activityReplace :: Maybe Asset
  , activityTranscode :: Maybe Asset
  }

