{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Party.Types
  ( PartyRow(..)
  , Party(..)
  , Account(..)
  , SiteAuth(..)
  , nobodySiteAuth
  , blankParty
  , blankAccount
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)

import Databrary.Has (Has(..))
import Databrary.Model.URL (URI)
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.ORCID

type instance IdType Party = Int32

data PartyRow = PartyRow
  { partyId :: Id Party
  , partySortName :: T.Text
  , partyPreName :: Maybe T.Text
  , partyORCID :: Maybe ORCID
  , partyAffiliation :: Maybe T.Text
  , partyURL :: Maybe URI
  } deriving (Eq, Show)

-- | Represents users, institutions, labs, *and* groups.
data Party = Party
  { partyRow :: !PartyRow
  , partyAccount :: Maybe Account
  -- , partySiteAccess :: Access -- site-level access this party is granted under root (currently SiteAuth only)
  , partyPermission :: Permission -- ^ permission current user has over this party
  , partyAccess :: Maybe Access -- ^ direct authorization this party has granted to current user
  } deriving (Eq, Show)

data Account = Account
  { accountEmail :: BS.ByteString
  , accountParty :: Party
  } deriving (Eq, Show)

-- makeHasRec ''PartyRow ['partyId]
instance Has (Id Party) PartyRow where
  view = partyId
-- makeHasRec ''Party ['partyRow]
instance Has PartyRow Party where
  view = partyRow
instance Has (Id Party) Party where
  view = (view . partyRow)
-- makeHasRec ''Account ['accountParty]
instance Has Party Account where
  view = accountParty
instance Has PartyRow Account where
  view = (view . accountParty)
instance Has (Id Party) Account where
  view = (view . accountParty)

instance Has Access Party where
  view Party{ partyAccess = Just a } = a
  view _ = mempty
instance Has Permission Party where
  view = partyPermission

instance Kinded Party where
  kindOf _ = "party"

-- Access to the site by a (current) account
data SiteAuth = SiteAuth
  { siteAccount :: Account -- maybe should be Party (for nobody)
  , accountPasswd :: Maybe BS.ByteString
  , siteAccess :: Access
  } deriving (Eq, Show)

-- makeHasRec ''SiteAuth ['siteAccount, 'siteAccess]
instance Has Account SiteAuth where
  view = siteAccount
instance Has Party SiteAuth where
  view = (view . siteAccount)
instance Has PartyRow SiteAuth where
  view = (view . siteAccount)
instance Has (Id Party) SiteAuth where
  view = (view . siteAccount)
instance Has Access SiteAuth where
  view = siteAccess

deriveLiftMany [''PartyRow, ''Party, ''Account]

-- this is unfortunate, mainly to avoid untangling Party.SQL
nobodySiteAuth :: SiteAuth
nobodySiteAuth = SiteAuth
  { siteAccount = Account
    { accountEmail = "nobody@databrary.org"
    , accountParty = Party
      { partyRow = PartyRow
        { partyId = Id (-1)
        , partySortName = "Nobody"
        , partyPreName = Nothing
        , partyORCID = Nothing
        , partyAffiliation = Nothing
        , partyURL = Nothing
        }
      , partyAccount = Nothing
      , partyPermission = PermissionREAD
      , partyAccess = Just minBound
      }
    }
  , accountPasswd = Nothing
  , siteAccess = mempty
  }

blankParty :: Party
blankParty = Party
  { partyRow = PartyRow
    { partyId = error "blankParty"
    , partySortName = ""
    , partyPreName = Nothing
    , partyORCID = Nothing
    , partyAffiliation = Nothing
    , partyURL = Nothing
    }
  , partyAccount = Nothing
  , partyPermission = PermissionNONE
  , partyAccess = Nothing
  }

blankAccount :: Account
blankAccount = Account
  { accountParty = blankParty{ partyAccount = Just blankAccount }
  , accountEmail = error "blankAccount"
  }
