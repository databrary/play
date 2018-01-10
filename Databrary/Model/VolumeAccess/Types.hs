{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Databrary.Model.VolumeAccess.Types
  ( VolumeAccess(..)
  -- , volumeAccessShareFull'
  , getShareFullDefault
  ) where

import Data.Int (Int16)
import qualified Language.Haskell.TH as TH

import Databrary.Has (makeHasFor)
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Party.Types

data VolumeAccess = VolumeAccess
  { volumeAccessIndividual, volumeAccessChildren :: Permission
  , volumeAccessSort :: Maybe Int16
  , volumeAccessShareFull :: Maybe Bool
  , volumeAccessParty :: Party
  , volumeAccessVolume :: Volume
  }

{-
volumeAccessShareFull' :: VolumeAccess -> Maybe Bool
volumeAccessShareFull' VolumeAccess{..} =
    if (getPartyId volumeAccessParty, volumeAccessIndividual) == (nobodyId, PermissionPUBLIC)
    then nobodyPublicDefault
    else generalDefault
  where
    getPartyId :: Party -> Id Party
    getPartyId = partyId . partyRow
    nobodyId :: Id Party
    nobodyId = (getPartyId . accountParty . siteAccount) nobodySiteAuth
    nobodyPublicDefault = Just True
    generalDefault = Nothing
-}

getShareFullDefault :: Party -> Permission -> Maybe Bool
getShareFullDefault targetParty individualAccessLevel =
    if (getPartyId targetParty, individualAccessLevel) == (nobodyId, PermissionPUBLIC)
    then nobodyPublicDefault
    else generalDefault
  where
    getPartyId :: Party -> Id Party
    getPartyId = partyId . partyRow
    nobodyId :: Id Party
    nobodyId = (getPartyId . accountParty . siteAccount) nobodySiteAuth
    nobodyPublicDefault = Just True
    generalDefault = Nothing

makeHasFor ''VolumeAccess
  [ ('volumeAccessVolume, TH.ConT ''Volume, [TH.ConT ''Id `TH.AppT` TH.ConT ''Volume])
  , ('volumeAccessParty, TH.ConT ''Party, [TH.ConT ''Id `TH.AppT` TH.ConT ''Party])
  ]
