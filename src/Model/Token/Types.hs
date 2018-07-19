{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Model.Token.Types
  ( Token(..)
  , AccountToken(..)
  , LoginToken(..)
  , Session(..)
  , Upload(..)
  , makeUpload
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)

import Has (Has(..))
import Model.Kind
import Model.Time
import Model.Id.Types
import Model.Party.Types
import Model.Permission.Types

type instance IdType Token = BS.ByteString

data Token = Token
  { tokenId :: Id Token
  , tokenExpires :: Timestamp
  }

instance Has (Id Token) Token where
  view = tokenId

data AccountToken = AccountToken
  { accountToken :: !Token
  , tokenAccount :: SiteAuth
  }

instance Has (Id Token) AccountToken where
  view = view . accountToken
instance Has SiteAuth AccountToken where
  view = tokenAccount
instance Has Access AccountToken where
  view = view . tokenAccount
instance Has (Id Party) AccountToken where
  view = view . tokenAccount
instance Has Account AccountToken where
  view = view . tokenAccount

data LoginToken = LoginToken
  { loginAccountToken :: !AccountToken
  , loginPasswordToken :: Bool
  }

-- these are signed version of Id Token
type instance IdType LoginToken = BS.ByteString

instance Kinded LoginToken where
  kindOf _ = "token"

instance Has (Id Token) LoginToken where
  view = view . loginAccountToken
instance Has SiteAuth LoginToken where
  view = view . loginAccountToken

data Session = Session
  { sessionAccountToken :: !AccountToken
  , sessionVerf :: !BS.ByteString
  , sessionSuperuser :: Bool
  }

instance Has (Id Token) Session where
  view = view . sessionAccountToken
instance Has Access Session where
  view = view . sessionAccountToken
instance Has (Id Party) Session where
  view = view . sessionAccountToken
instance Has Account Session where
  view = view . sessionAccountToken

data Upload = Upload
  { uploadAccountToken :: AccountToken
  , uploadFilename :: BS.ByteString
  , uploadSize :: Int64
  }

instance Has (Id Token) Upload where
  view = view . uploadAccountToken

makeUpload :: Token -> BS.ByteString -> Int64 -> SiteAuth -> Upload
makeUpload t n z u = Upload (AccountToken t u) n z
