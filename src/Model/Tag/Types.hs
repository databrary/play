{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies, DataKinds, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Model.Tag.Types
  ( TagName(..)
  , validateTag
  , Tag(..)
  , TagUse(..)
  , TagUseRow(..)
  , TagCoverage(..)
  , TagWeight(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable)
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import qualified Text.Regex.Posix as Regex
import qualified Web.Route.Invertible as R

import Ops
import Has (Has(..))
import qualified JSON as JSON
import Model.Kind
import Model.Id.Types
import Model.Party.Types
import Model.Container.Types
import Model.Segment
import Model.Slot.Types
import Model.Volume.Types

newtype TagName = TagName { tagNameBS :: BS.ByteString } deriving (JSON.ToJSON, {- JSON.FromJSON, -} Typeable {-, Show, Eq -})

validTag :: Regex.Regex
validTag = Regex.makeRegex
  ("^[a-z][-a-z ]+[a-z]$" :: BS.ByteString)

validateTag :: BS.ByteString -> Maybe TagName
validateTag t = Regex.matchTest validTag tt `thenUse` TagName tt where
  tt = BSC.map toLower $ BSC.unwords $ BSC.words t

instance R.Parameter R.PathString TagName where
  parseParameter = validateTag . TE.encodeUtf8
  renderParameter = TE.decodeLatin1 . tagNameBS

instance PGParameter "character varying" TagName where
  pgEncode t (TagName n) = pgEncode t n
  pgEncodeValue e t (TagName n) = pgEncodeValue e t n
  pgLiteral t (TagName n) = pgLiteral t n
instance PGColumn "character varying" TagName where
  pgDecode t = TagName . pgDecode t
  pgDecodeValue e t = TagName . pgDecodeValue e t

type instance IdType Tag = Int32

data Tag = Tag
  { tagId :: Id Tag
  , tagName :: TagName
  }

instance Kinded Tag where
  kindOf _ = "tag"

data TagUse = TagUse
  { useTag :: Tag
  , tagKeyword :: Bool
  , tagWho :: Account
  , tagSlot :: Slot
  }

instance Has (Id Container) TagUse where
  view = view . tagSlot
instance Has Model.Volume.Types.Volume TagUse where
  view = view . tagSlot
instance Has (Id Model.Volume.Types.Volume) TagUse where
  view = volumeId . volumeRow . containerVolume . slotContainer . tagSlot

data TagUseRow = TagUseRow
  { useTagRow :: Tag
  , tagRowKeyword :: Bool
  , tagRowWhoId :: Id Party
  , tagRowSlotId :: SlotId
  }

data TagWeight = TagWeight
  { tagWeightTag :: Tag
  , tagWeightWeight :: Int32
  }

data TagCoverage = TagCoverage
  { tagCoverageWeight :: !TagWeight
  , tagCoverageContainer :: Container
  , tagCoverageSegments
  , tagCoverageKeywords
  , tagCoverageVotes :: [Segment]
  }
