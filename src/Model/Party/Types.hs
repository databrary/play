{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Model.Party.Types
  ( PartyRow(..)
  , Party(..)
  , Loaded(..)
  , loadedToMaybe
  , Account(..)
  , getPartyId
  , SiteAuth(..)
  , nobodyParty
  , rootParty
  , staffParty
  , nobodySiteAuth
  , blankParty
  , blankAccount
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)

import Has (Has(..))
import Model.URL (URI)
import Model.Kind
import Model.Id.Types
import Model.Permission.Types
import Model.ORCID

type instance IdType Party = Int32

data PartyRow = PartyRow
  { partyId :: Id Party
  , partySortName :: T.Text
  , partyPreName :: Maybe T.Text
  , partyORCID :: Maybe ORCID
  , partyAffiliation :: Maybe T.Text
  , partyURL :: Maybe URI
  } -- deriving (Show) -- (Eq)

-- | Represents users, institutions, labs, *and* groups.
data Party = Party
  { partyRow :: !PartyRow
  , partyAccount :: Maybe Account
  -- , partySiteAccess :: Access -- site-level access this party is granted under root (currently SiteAuth only)
  , partySiteAccess :: !(Loaded Permission) -- ^ site-level data access this party is granted under root party (0).
                                           -- Maybe is only an indication of whether this value was loaded.
  , partyPermission :: Permission -- ^ permission current user has over this party
  , partyAccess :: Maybe Access -- ^ direct authorization this party has granted to current user
  }

-- | When loading a graph of objects, some queries will neglect loaded
-- all related objects. Use this type to indicate an object which isn't loaded
-- by all queries
data Loaded a = -- TODO: move this to a utility module when used more widely
    Loaded a
  | NotLoaded

-- | Transform a Loaded value into the a Maybe value
loadedToMaybe :: Loaded a -> Maybe a
loadedToMaybe (Loaded v) = Just v
loadedToMaybe NotLoaded = Nothing

data Account = Account
  { accountEmail :: BS.ByteString
  , accountParty :: Party
  }

instance Has (Id Party) Party where
  view = getPartyId
getPartyId :: Party -> Id Party
getPartyId = partyId . partyRow
instance Has Party Account where
  view = accountParty
instance Has (Id Party) Account where
  view = (getPartyId . accountParty)

instance Has Access Party where
  view Party{ partyAccess = Just a } = a
  view _ = mempty

instance Kinded Party where
  kindOf _ = "party"

-- | TODO: clarify. This is not necessarily a session, but... some user (human
-- being) who has been granted access to the site. There is a corner case
-- indirection because sometimes a job runs a human being.
data SiteAuth = SiteAuth
  { siteAccount :: Account -- ^ maybe should be Party (for nobody)
  , accountPasswd :: Maybe BS.ByteString
  , siteAccess :: Access -- ^ Still figuring out what an 'Access' is.
  }

instance Has Account SiteAuth where
  view = siteAccount
instance Has Party SiteAuth where
  view = (view . siteAccount)
instance Has (Id Party) SiteAuth where
  view = (view . siteAccount)
instance Has Access SiteAuth where
  view = siteAccess

deriveLiftMany [''PartyRow, ''Party, ''Account, ''Loaded]

-- The values below assume a minimalist loading of each object, with no
-- related objects loaded.
nobodyParty, rootParty, staffParty :: Party -- TODO: load on startup from service module
nobodyParty =
   Party
         (PartyRow (Id (-1)) (T.pack "Everybody") Nothing Nothing Nothing Nothing)
         Nothing
         NotLoaded
         PermissionREAD
         Nothing
rootParty =
   Party
         (PartyRow (Id 0) (T.pack "Databrary") Nothing Nothing Nothing Nothing)
         Nothing
         NotLoaded
         PermissionSHARED
         Nothing
staffParty =
   Party
         (PartyRow (Id 2) (T.pack "Staff") Nothing Nothing (Just (T.pack "Databrary")) Nothing)
         Nothing
         NotLoaded
         PermissionPUBLIC
         Nothing

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
      , partySiteAccess = NotLoaded
      , partyPermission = PermissionREAD
      , partyAccess = Just minBound
      }
    }
  , accountPasswd = Nothing
  , siteAccess = mempty
  }

-- | Uninitialized Party object to be used in creating new parties (and accounts)
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
  , partySiteAccess = NotLoaded
  , partyPermission = PermissionNONE
  , partyAccess = Nothing
  }

-- | Uninitialized Account object to be used in creating new accounts
blankAccount :: Account
blankAccount = Account
  { accountParty = blankParty{ partyAccount = Just blankAccount }
  , accountEmail = error "blankAccount"
  }
