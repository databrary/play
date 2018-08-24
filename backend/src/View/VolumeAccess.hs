{-# LANGUAGE OverloadedStrings #-}
module View.VolumeAccess
  ( volumeAccessTitle
  , volumeAccessPresetTitle
  -- , htmlVolumeAccessForm
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import qualified Data.Text as T

import qualified Store.Config as C
import Service.Messages
-- import Action
-- import Model.Party
import Model.Permission
-- import Model.Volume
-- import Model.VolumeAccess
-- import Controller.Paths
-- import View.Form

-- import {-# SOURCE #-} Controller.VolumeAccess

volumeAccessTitle :: Permission -> Messages -> T.Text
volumeAccessTitle perm = getMessage $ C.Path ["access", "edit", BSC.pack (show perm), "title"]

volumeAccessPresetTitle :: Bool -> Messages -> T.Text
volumeAccessPresetTitle shared = getMessage $ C.Path ["access", "preset", "title" <> BSC.pack (show (fromEnum shared))]

{-
htmlVolumeAccessForm :: VolumeAccess -> RequestContext -> FormHtml f
htmlVolumeAccessForm a@VolumeAccess{ volumeAccessVolume = vol, volumeAccessParty = p } = htmlForm
  ("Access to " <> volumeName (volumeRow vol) <> " for " <> partyName (partyRow p))
  postVolumeAccess (HTML, (volumeId $ volumeRow vol, VolumeAccessTarget $ partyId $ partyRow p))
  (do
    field "individual" $ inputEnum True $ Just $ volumeAccessIndividual a
    field "children" $ inputEnum True $ Just $ volumeAccessChildren a
    field "delete" $ inputCheckbox False)
  (const mempty)
-}
