{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Controller.Container
  ( getContainer
  , viewContainer
  , viewContainerEdit
  , createContainer
  , postContainer
  , deleteContainer
  , containerDownloadName
  ) where

import Control.Arrow (second)
import Control.Monad (when, unless, mfilter)
import qualified Data.Invertible as I
import Data.Maybe (fromMaybe, maybeToList, isJust)
import qualified Data.Text as T
import Network.HTTP.Types (noContent204, movedPermanently301, conflict409)
import qualified Web.Route.Invertible as R

import Databrary.Has
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Model.Release
import Databrary.Model.Notification.Types
import Databrary.Action.Response
import Databrary.Action
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Controller.Paths
import Databrary.Controller.Permission
import Databrary.Controller.Form
import Databrary.Controller.Angular
import Databrary.Controller.Volume
import Databrary.Controller.Notification
import {-# SOURCE #-} Databrary.Controller.Slot
-- import Databrary.View.Container
import Databrary.View.Form (FormHtml)

getContainer :: Permission -> Maybe (Id Volume) -> Id Slot -> Bool -> Handler Container
getContainer p mv (Id (SlotId i s)) top
  | segmentFull s = do
    c <- checkPermission p =<< maybeAction . maybe id (\v -> mfilter $ (v ==) . view) mv =<< lookupContainer i
    unless top $ do
      t <- lookupVolumeTopContainer (containerVolume c)
      when (containerId (containerRow c) == containerId (containerRow t)) $ result =<< peeks notFoundResponse
    return c
  | otherwise = result =<< peeks notFoundResponse

containerDownloadName :: Container -> [T.Text]
containerDownloadName Container{ containerRow = ContainerRow{..} } =
  (if containerTop then ("materials" :) else id) $
    T.pack (show containerId) : maybeToList containerName

viewContainer :: ActionRoute (API, (Maybe (Id Volume), Id Container))
viewContainer = second (second $ slotContainerId . unId I.:<->: containerSlotId) `R.mapActionRoute` (viewSlot False)

containerForm :: Container -> DeformHandler () Container
containerForm c = do
  csrfForm
  name <- "name" .:> deformOptional (deformNonEmpty deform)
  top <- "top" .:> deformOptional deform
  date <- "date" .:> deformOptional (deformNonEmpty deform)
  release <- "release" .:> deformOptional (deformNonEmpty deform)
  return c
    { containerRow = (containerRow c)
      { containerName = fromMaybe (containerName $ containerRow c) name
      , containerTop = fromMaybe (containerTop $ containerRow c) top
      , containerDate = fromMaybe (containerDate $ containerRow c) date
      }
    , containerRelease = fromMaybe (containerRelease c) release
    }

viewContainerEdit :: ActionRoute (Maybe (Id Volume), Id Slot)
viewContainerEdit = action GET (pathHTML >/> pathMaybe pathId </> pathSlotId </< "edit") $ \(vi, ci) -> withAuth $ do
  when (isJust vi) $ angular
  c <- getContainer PermissionEDIT vi ci False
  unless (isJust vi) $
    result =<< peeks (redirectRouteResponse movedPermanently301 [] viewContainerEdit (Just (view c), containerSlotId (view c)))
  return $ okResponse [] $ ("" :: String) -- should never get here
  -- peeks $ blankForm . htmlContainerEdit (Right c)

createContainer :: ActionRoute (Id Volume)
createContainer = action POST (pathJSON >/> pathId </< "slot") $ \vi -> withAuth $ do
  vol <- getVolume PermissionEDIT vi
  bc <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ containerForm (blankContainer vol)
  c <- addContainer bc
  -- TODO: NoticeReleaseSlot?
  -- case api of
  return $ okResponse [] $ JSON.recordEncoding $ containerJSON False c -- False because level EDIT
  -- HTML -> peeks $ otherRouteResponse [] viewContainer (api, (Just vi, containerId $ containerRow c))

postContainer :: ActionRoute (Id Slot)
postContainer = action POST (pathJSON >/> pathSlotId) $ \(ci) -> withAuth $ do
  c <- getContainer PermissionEDIT Nothing ci False
  c' <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ containerForm c
  changeContainer c'
  when (containerRelease c' /= containerRelease c) $ do
    r <- changeRelease (containerSlot c') (containerRelease c')
    unless r $
      result $ emptyResponse conflict409 []
    when (containerRelease c' == Just ReleasePUBLIC && not (containerTop $ containerRow c')) $
      createVolumeNotification (containerVolume c) $ \n -> (n NoticeReleaseSlot)
        { notificationContainerId = Just $ containerId $ containerRow c'
        , notificationRelease = containerRelease c'
        }
  return $ okResponse [] $ JSON.recordEncoding $ containerJSON False c' -- False because level EDIT
  --HTML -> peeks $ otherRouteResponse [] (viewSlot False) (api, (Just (view c'), ci))

deleteContainer :: ActionRoute (Id Slot)
deleteContainer = action DELETE (pathJSON >/> pathSlotId) $ \ci -> withAuth $ do
  guardVerfHeader
  c <- getContainer PermissionEDIT Nothing ci False
  r <- removeContainer c
  unless r $ result $ 
    response conflict409 [] $ JSON.recordEncoding $ containerJSON False c -- False because level EDIT
    -- HTML -> response conflict409 [] ("This container is not empty." :: T.Text)
  return $ emptyResponse noContent204 []
  -- HTML -> peeks $ otherRouteResponse [] viewVolume (api, view c)
