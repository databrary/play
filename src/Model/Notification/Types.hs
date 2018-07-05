{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Model.Notification.Types
  ( module Model.Notification.Notice
  , Notification(..)
  , blankNotification
  ) where

import Model.Time
import Model.Id.Types
import Model.Kind
import Model.Party
import Model.Volume.Types
import Model.Container.Types
import Model.Segment
import Model.Asset.Types
-- import Model.Comment.Types
import Model.Tag.Types
import Model.Permission.Types
import Model.Release.Types
import Model.Notification.Notice

type instance IdType Notification = Int32

data Notification = Notification
  { notificationId :: Id Notification
  , notificationTarget :: !Account
  , notificationNotice :: !Notice
  , notificationTime :: Timestamp
  , notificationDelivered :: !Delivery
  , notificationAgent :: PartyRow
  , notificationParty :: Maybe PartyRow
  , notificationVolume :: Maybe VolumeRow
  , notificationPermission :: Maybe Permission
  , notificationContainerId :: Maybe (Id Container)
  , notificationSegment :: Maybe Segment
  , notificationAssetId :: Maybe (Id Asset)
  , notificationRelease :: Maybe Release
  -- , notificationCommentId :: Maybe (Id Comment)
  , notificationTag :: Maybe Tag
  }

instance Kinded Notification where
  kindOf _ = "notification"

blankNotification :: Account -> Notice -> Notification
blankNotification target notice = Notification
  { notificationId = error "blankNotification"
  , notificationTarget = target
  , notificationNotice = notice
  , notificationTime = error "blankNotification"
  , notificationDelivered = DeliveryNone
  , notificationAgent = partyRow nobodyParty
  , notificationParty = Nothing
  , notificationVolume = Nothing
  , notificationPermission = Nothing
  , notificationContainerId = Nothing
  , notificationSegment = Nothing
  , notificationAssetId = Nothing
  , notificationRelease = Nothing
  -- , notificationCommentId = Nothing
  , notificationTag = Nothing
  }
