{-# LANGUAGE OverloadedStrings #-}
module Controller.VolumeState
  ( postVolumeState
  , deleteVolumeState
  ) where

import Data.Maybe (fromMaybe)
import Network.HTTP.Types (noContent204)
import qualified Web.Route.Invertible as R

import qualified Data.Aeson as Aeson
import Model.Id
import Model.Permission
import Model.Volume
import Model.VolumeState
import HTTP.Path.Parser
import HTTP.Form.Deform
import Action.Route
import Action
import Controller.Form
import Controller.Paths
import Controller.Volume

data CreateOrUpdateVolumeStateRequest = CreateOrUpdateVolumeStateRequest Aeson.Value Bool

postVolumeState :: ActionRoute (Id Volume, VolumeStateKey)
postVolumeState = action PUT (pathJSON >/> pathId </> "state" >/> R.parameter) $ \(vi, k) -> withAuth $ do
  _ <- getVolume PermissionEDIT vi
  s <- runForm Nothing $ do
    j <- deform
    p <- "public" .:> fromMaybe False <$> deformOptional deform
    let _ = CreateOrUpdateVolumeStateRequest j p
    return VolumeState
      { stateVolumeId = vi
      , volumeStateKey = k
      , volumeStatePublic = p
      , volumeStateValue = j
      }
  changeVolumeState s
  return $ emptyResponse noContent204 []

deleteVolumeState :: ActionRoute (Id Volume, VolumeStateKey)
deleteVolumeState = action DELETE (pathJSON >/> pathId </> "state" >/> R.parameter) $ \(vi, k) -> withAuth $ do
  _ <- getVolume PermissionEDIT vi
  r <- DeleteVolumeStateResponse <$> removeVolumeState vi k
  return $ okResponse [] $ (Aeson.encode . wasDeleted) r

newtype DeleteVolumeStateResponse = DeleteVolumeStateResponse { wasDeleted :: Bool }
