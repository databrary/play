{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Release.Types
  ( Release(..)
  , EffectiveRelease(..)
  ) where

import Data.Foldable (fold)
import Language.Haskell.TH.Lift (deriveLift)
import qualified Data.Typeable.Internal
import qualified GHC.Arr
import qualified Database.PostgreSQL.Typed.Types
import qualified Database.PostgreSQL.Typed.Dynamic
import qualified Database.PostgreSQL.Typed.Enum
import qualified Data.Aeson.Types
import qualified Data.ByteString
import qualified Data.ByteString.Char8

import Databrary.Has (Has(..))
import Databrary.Model.Enum
import qualified Databrary.Model.Kind
import qualified Databrary.HTTP.Form.Deform

-- makeDBEnum "release" "Release"
-- TODO: db coherence
data Release
    = ReleasePRIVATE | ReleaseSHARED | ReleaseEXCERPTS | ReleasePUBLIC
          deriving (Eq,
                    Ord,
                    Enum,
                    GHC.Arr.Ix,
                    Bounded,
                    Data.Typeable.Internal.Typeable)
instance Show Release where
  show ReleasePRIVATE = "PRIVATE"
  show ReleaseSHARED = "SHARED"
  show ReleaseEXCERPTS = "EXCERPTS"
  show ReleasePUBLIC = "PUBLIC"
instance Database.PostgreSQL.Typed.Types.PGType "release"
instance Database.PostgreSQL.Typed.Types.PGParameter "release" Release where
  pgEncode _ ReleasePRIVATE
    = Data.ByteString.pack [80, 82, 73, 86, 65, 84, 69]
  pgEncode _ ReleaseSHARED
    = Data.ByteString.pack [83, 72, 65, 82, 69, 68]
  pgEncode _ ReleaseEXCERPTS
    = Data.ByteString.pack [69, 88, 67, 69, 82, 80, 84, 83]
  pgEncode _ ReleasePUBLIC
    = Data.ByteString.pack [80, 85, 66, 76, 73, 67]
instance Database.PostgreSQL.Typed.Types.PGColumn "release" Release where
  pgDecode _ x_a2Skv
    = case Data.ByteString.unpack x_a2Skv of
        [80, 82, 73, 86, 65, 84, 69] -> ReleasePRIVATE
        [83, 72, 65, 82, 69, 68] -> ReleaseSHARED
        [69, 88, 67, 69, 82, 80, 84, 83] -> ReleaseEXCERPTS
        [80, 85, 66, 76, 73, 67] -> ReleasePUBLIC
        _ -> error
               ("pgDecode release: " ++ (Data.ByteString.Char8.unpack x_a2Skv))
instance Database.PostgreSQL.Typed.Dynamic.PGRep "release" Release
instance Database.PostgreSQL.Typed.Enum.PGEnum Release
instance Databrary.Model.Kind.Kinded Release where
  kindOf _ = "release"
instance DBEnum Release
instance Data.Aeson.Types.ToJSON Release where
  toJSON
    = (Data.Aeson.Types.toJSON . fromEnum)
instance Data.Aeson.Types.FromJSON Release where
  parseJSON = parseJSONEnum
instance Databrary.HTTP.Form.Deform.Deform f_a2Skw Release where
  deform = enumForm

instance Monoid Release where
  mempty = ReleasePRIVATE
  mappend = max

instance Has Release (Maybe Release) where
  view = fold

deriveLift ''Release

data EffectiveRelease =
       EffectiveRelease {
           effRelPublic :: !Release
         , effRelPrivate :: !Release
         } deriving (Eq, Show)
