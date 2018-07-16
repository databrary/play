{-# LANGUAGE OverloadedStrings #-}
module Controller.Activity
  ( viewSiteActivityHandler
  , viewPartyActivity
  , viewVolumeActivity
  , viewContainerActivity
  ) where

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.IORef (readIORef)
import Data.List (nubBy)
import Data.Maybe (isJust, mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)

import Ops
import Has
import qualified JSON as JSON
import Service.Types
import Model.Id
import Model.Permission
import Model.Party
import Model.Authorize
import Model.Volume hiding (getVolume)
import Model.VolumeAccess
import Model.Slot
import Model.Activity
import HTTP.Path.Parser
import Action
import Controller.Paths
import Controller.Angular
import Controller.Party
import Controller.Volume
import Controller.Container

viewSiteActivityHandler :: Action -- TODO: GET only
viewSiteActivityHandler = withAuth $ do
  ss <- focusIO $ readIORef . serviceStats
  vl <- map (second $ ("volume" JSON..=:) . (\v -> volumeJSONSimple v)) . nubBy ((==) `on` volumeId . volumeRow . snd) <$> lookupVolumeShareActivity 8
  al <- map (second $ ("party"  JSON..=:) . partyJSON)  . nubBy ((==) `on` partyId  . partyRow  . snd) <$> lookupAuthorizeActivity 8
  return
    $ okResponse []
      $ JSON.pairs $
           "stats" JSON..= ss
        <> JSON.nestObject "activity" (\u -> map (u . ent) (take 12 $ mergeBy ((fo .) . comparing fst) vl al))
    -- HTML -> peeks $ okResponse [] . htmlSiteActivity ss
  where
  ent (t, j) = j <> "time" JSON..= t
  fo GT = LT
  fo _ = GT

viewPartyActivity :: ActionRoute (API, PartyTarget)
viewPartyActivity = action GET (pathAPI </> pathPartyTarget </< "activity") $ \(api, p) -> withAuth $ do
  when (api == HTML) angular
  v <- getParty (Just PermissionADMIN) p
  a <- lookupPartyActivity v
  return $ case api of
    ~JSON -> okResponse [] $ JSON.toEncoding $ mapMaybe activityJSON a
    -- TODO: HTML

viewVolumeActivity :: ActionRoute (API, Id Volume)
viewVolumeActivity = action GET (pathAPI </> pathId </< "activity") $ \(api, vi) -> withAuth $ do
  when (api == HTML) angular
  v <- getVolume PermissionEDIT vi
  a <- lookupVolumeActivity v
  return $ case api of
    ~JSON -> okResponse [] $ JSON.toEncoding $ mapMaybe activityJSON a
    -- TODO: HTML

viewContainerActivity :: ActionRoute (API, (Maybe (Id Volume), Id Slot))
viewContainerActivity = action GET (pathAPI </> pathMaybe pathId </> pathSlotId </< "activity") $ \(api, (vi, ci)) -> withAuth $ do
  when (api == HTML && isJust vi) angular
  v <- getContainer PermissionEDIT vi ci True
  a <- lookupContainerActivity v
  (liftIO . print) "before activity json"
  return $ case api of
    ~JSON -> okResponse [] $ JSON.toEncoding $ mapMaybe activityJSON a
    -- TODO: HTML
