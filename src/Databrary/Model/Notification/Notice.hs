{-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Notification.Notice
  ( Delivery(..)
  , fromMaybeDelivery
  , periodicDelivery
  , Notice(..)
  , noticeId
  , getNotice
  , getNotice'
  ) where

import qualified Data.Aeson.Types as JSON
import Control.Arrow (left)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int16)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import Database.PostgreSQL.Typed.Dynamic (PGRep)
import qualified Data.Typeable.Internal
import qualified GHC.Arr
import qualified Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Enum
import qualified Data.ByteString
import qualified Data.ByteString.Char8

import Databrary.HTTP.Form (FormDatum(..))
import Databrary.HTTP.Form.Deform
import Databrary.Model.Kind
import Databrary.Model.Id
import Databrary.Model.Enum
import Databrary.Model.Periodic
import Databrary.Model.Notification.Boot

-- makeDBEnum "notice_delivery" "Delivery"
data Delivery
  = DeliveryNone |
    DeliverySite |
    DeliveryWeekly |
    DeliveryDaily |
    DeliveryAsync
  deriving (Eq,
            Ord,
            Enum,
            GHC.Arr.Ix,
            Bounded,
            Data.Typeable.Internal.Typeable)
instance Show Delivery where
  show DeliveryNone = "none"
  show DeliverySite = "site"
  show DeliveryWeekly = "weekly"
  show DeliveryDaily = "daily"
  show DeliveryAsync = "async"
instance Database.PostgreSQL.Typed.Types.PGType "notice_delivery"
instance PGParameter "notice_delivery" Delivery where
  pgEncode _ DeliveryNone = Data.ByteString.pack [110, 111, 110, 101]
  pgEncode _ DeliverySite = Data.ByteString.pack [115, 105, 116, 101]
  pgEncode _ DeliveryWeekly
    = Data.ByteString.pack [119, 101, 101, 107, 108, 121]
  pgEncode _ DeliveryDaily
    = Data.ByteString.pack [100, 97, 105, 108, 121]
  pgEncode _ DeliveryAsync
    = Data.ByteString.pack [97, 115, 121, 110, 99]
instance PGColumn "notice_delivery" Delivery where
  pgDecode _ x_a42l0
    = case Data.ByteString.unpack x_a42l0 of
        [110, 111, 110, 101] -> DeliveryNone
        [115, 105, 116, 101] -> DeliverySite
        [119, 101, 101, 107, 108, 121] -> DeliveryWeekly
        [100, 97, 105, 108, 121] -> DeliveryDaily
        [97, 115, 121, 110, 99] -> DeliveryAsync
        _ -> error ("pgDecode notice_delivery: " ++ (BSC.unpack x_a42l0))
instance PGRep "notice_delivery" Delivery
instance Database.PostgreSQL.Typed.Enum.PGEnum Delivery
instance Kinded Delivery where
  kindOf _ = "notice_delivery"
instance DBEnum Delivery
instance JSON.ToJSON Delivery where
  toJSON = (JSON.toJSON . fromEnum)
instance JSON.FromJSON Delivery where
  parseJSON = parseJSONEnum
instance Deform f_a42l1 Delivery where
  deform = enumForm

fromMaybeDelivery :: Maybe Delivery -> Delivery
fromMaybeDelivery (Just d) = d
fromMaybeDelivery Nothing = DeliveryNone

periodicDelivery :: Maybe Period -> Delivery
periodicDelivery (Just PeriodDaily) = DeliveryDaily
periodicDelivery (Just PeriodWeekly) = DeliveryWeekly
periodicDelivery Nothing = DeliveryAsync

makeNotice

noticeFromId' :: Int16 -> Notice
noticeFromId' = fromMaybe (error "noticeFromId'") . noticeFromId

instance PGParameter "smallint" Notice where
  pgEncode t = pgEncode t . noticeToId
  pgEncodeValue e t = pgEncodeValue e t . noticeId
  pgLiteral t = pgLiteral t . noticeToId
instance PGColumn "smallint" Notice where
  pgDecode t = noticeFromId' . pgDecode t
  pgDecodeValue e t = noticeFromId' . pgDecodeValue e t
instance PGRep "smallint" Notice

type instance IdType Notice = Int16

noticeId :: Notice -> Id Notice
noticeId = Id . noticeToId

getNotice :: Id Notice -> Maybe Notice
getNotice (Id i) = noticeFromId i

getNotice' :: Id Notice -> Notice
getNotice' = fromMaybe (error "getNotice'") . getNotice

instance Kinded Notice where
  kindOf _ = "notice"
instance JSON.ToJSON Notice where
  toJSON = JSON.toJSON . noticeToId
instance JSON.FromJSON Notice where
  parseJSON (JSON.String t) | Just e <- noticeFromName (T.unpack t) = return e
  parseJSON (JSON.Number x) = maybe (fail "notice out of range") return $ noticeFromId (round x)
  parseJSON _ = fail "Invalid notice"
instance Deform f Notice where
  deform = deformParse minBound fv where
    fv (FormDatumBS b) = maybe (fail "Invalid notice") return $ noticeFromName $ BSC.unpack b
    fv (FormDatumJSON j) = left T.pack $ JSON.parseEither JSON.parseJSON j
    fv _ = fail "Invalid notice"
