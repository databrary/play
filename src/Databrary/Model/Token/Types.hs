{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Token.Types
  ( Token(..)
  , AccountToken(..)
  , LoginToken(..)
  , Session(..)
  , Upload(..)
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)

import Databrary.Has (Has(..))
import Databrary.Model.Kind
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Permission.Types

type instance IdType Token = BS.ByteString

data Token = Token
  { tokenId :: Id Token
  , tokenExpires :: Timestamp
  }

-- makeHasRec ''Token ['tokenId]
instance Has (Id Token) Token where
  view = tokenId

data AccountToken = AccountToken
  { accountToken :: !Token
  , tokenAccount :: SiteAuth
  }

-- makeHasRec ''AccountToken ['accountToken, 'tokenAccount]
instance Has Token AccountToken where
  view = accountToken
instance Has (Id Token) AccountToken where
  view = (view . accountToken)
instance Has SiteAuth AccountToken where
  view = tokenAccount
instance Has Access AccountToken where
  view = (view . tokenAccount)
instance Has (Id Party) AccountToken where
  view = (view . tokenAccount)
instance Has PartyRow AccountToken where
  view = (view . tokenAccount)
instance Has Party AccountToken where
  view = (view . tokenAccount)
instance Has Account AccountToken where
  view = (view . tokenAccount)

data LoginToken = LoginToken
  { loginAccountToken :: !AccountToken
  , loginPasswordToken :: Bool
  }

-- these are signed version of Id Token
type instance IdType LoginToken = BS.ByteString

instance Kinded LoginToken where
  kindOf _ = "token"

-- makeHasRec ''LoginToken ['loginAccountToken]
instance Has AccountToken LoginToken where
  view = loginAccountToken
instance Has Token LoginToken where
  view = (view . loginAccountToken)
instance Has (Id Token) LoginToken where
  view = (view . loginAccountToken)
instance Has SiteAuth LoginToken where
  view = (view . loginAccountToken)
instance Has Access LoginToken where
  view = (view . loginAccountToken)
instance Has (Id Party) LoginToken where
  view = (view . loginAccountToken)
instance Has PartyRow LoginToken where
  view = (view . loginAccountToken)
instance Has Party LoginToken where
  view = (view . loginAccountToken)
instance Has Account LoginToken where
  view = (view . loginAccountToken)

data Session = Session
  { sessionAccountToken :: !AccountToken
  , sessionVerf :: !BS.ByteString
  , sessionSuperuser :: Bool
  }

-- makeHasRec ''Session ['sessionAccountToken]
instance Has AccountToken Session where
  view = sessionAccountToken
instance Has Token Session where
  view = (view . sessionAccountToken)
instance Has (Id Token) Session where
  view = (view . sessionAccountToken)
instance Has SiteAuth Session where
  view = (view . sessionAccountToken)
instance Has Access Session where
  view = (view . sessionAccountToken)
instance Has (Id Party) Session where
  view = (view . sessionAccountToken)
instance Has PartyRow Session where
  view = (view . sessionAccountToken)
instance Has Party Session where
  view = (view . sessionAccountToken)
instance Has Account Session where
  view = (view . sessionAccountToken)

data Upload = Upload
  { uploadAccountToken :: AccountToken
  , uploadFilename :: BS.ByteString
  , uploadSize :: Int64
  }

-- makeHasRec ''Upload ['uploadAccountToken]
instance Has AccountToken Upload where
  view = uploadAccountToken
instance Has Token Upload where
  view = (view . uploadAccountToken)
instance Has (Id Token) Upload where
  view = (view . uploadAccountToken)
instance Has SiteAuth Upload where
  view = (view . uploadAccountToken)
instance Has Access Upload where
  view = (view . uploadAccountToken)
instance Has (Id Party) Upload where
  view = (view . uploadAccountToken)
instance Has PartyRow Upload where
  view = (view . uploadAccountToken)
instance Has Party Upload where
  view = (view . uploadAccountToken)
instance Has Account Upload where
  view = (view . uploadAccountToken)
