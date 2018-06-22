{-# LANGUAGE TemplateHaskell #-}
module Model.Notification.SQL
  ( selectNotifyDelivery
  , selectTargetNotification
  , selectNotification
  , selectPartyAuthorizationNotify
  ) where

import qualified Language.Haskell.TH as TH

import Has
import Model.SQL.Select
import Model.Time
import Model.Id.Types
import Model.Permission.Types
import Model.Release.Types
import Model.Party.Types
import Model.Party.SQL
import Model.Volume.Types
import Model.Volume.SQL
import Model.Container.Types
import Model.Segment
import Model.Asset.Types
import Model.Tag.Types
import Model.Tag.SQL
import Model.Comment.Types
import Model.Notification.Types

selectNotifyDelivery :: Selector
selectNotifyDelivery = selectMap (TH.VarE 'fromMaybeDelivery `TH.AppE`) $ selectColumn "notify_view" "delivery"

makePartyAuthorizationNotice :: (Party, Maybe Permission) -> Delivery -> (Party, Maybe Permission, Delivery)
makePartyAuthorizationNotice (p, a) d = (p, a, d)

selectPartyAuthorizationNotify :: TH.Name -- ^ 'Identity'
  -> Selector -- ^ @('Party', Maybe 'Permission', 'Delivery')@
selectPartyAuthorizationNotify ident = selectJoin 'makePartyAuthorizationNotice
  [ selectPartyAuthorization ident
  , joinOn "id = target"
    selectNotifyDelivery
  ]

makeNotification :: Id Notification -> Notice -> Timestamp -> Delivery -> Maybe Permission -> Maybe (Id Container) -> Maybe Segment -> Maybe (Id Asset) -> Maybe Release -> Maybe (Id Comment) -> PartyRow -> Maybe PartyRow -> Maybe VolumeRow -> Maybe Tag -> Account -> Notification
makeNotification i n t d e c s a r m w p v g u = Notification i (view u) n t d w p v e c s a r m g

notificationRow :: Selector -- ^ @'PartyRow' -> Maybe 'PartyRow' -> Maybe 'VolumeRow' -> Maybe 'Tag' -> 'Account' -> 'Notification'@
notificationRow = selectColumns 'makeNotification "notification" ["id", "notice", "time", "delivered", "permission", "container", "segment", "asset", "release", "comment"]

selectTargetNotification :: Selector -- ^ @'Account' -> 'Notification'@
selectTargetNotification = selectJoin '($)
  [ notificationRow
  , joinOn "notification.agent = agent.id"
    $ selectPartyRow `fromAlias` "agent"
  , maybeJoinOn "notification.party = nparty.id"
    $ selectPartyRow `fromAlias` "nparty"
  , maybeJoinOn "notification.volume = volume.id"
    $ selectVolumeRow
  , maybeJoinOn "notification.tag = tag.id"
    $ selectTag
  ]

selectNotification :: Selector -- ^ @'Notification'@
selectNotification = selectJoin '($)
  [ selectTargetNotification
  , joinOn "notification.target = account.id"
    selectUserAccount
  ]
