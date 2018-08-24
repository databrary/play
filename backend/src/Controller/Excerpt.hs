{-# LANGUAGE OverloadedStrings #-}
module Controller.Excerpt
  ( postExcerpt
  , deleteExcerpt
  ) where

import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.HTTP.Types (conflict409)

import Has
import qualified JSON
import Model.Id
import Model.Permission
import Model.Release (EffectiveRelease(..))
import Model.Slot
import Model.Asset
import Model.AssetSegment
import Model.Excerpt
import Model.Notification.Types
import Model.Release.Types
import HTTP.Form.Deform
import HTTP.Path.Parser
import Action
import Controller.Permission
import Controller.Paths
import Controller.Form
import Controller.Notification
import Controller.AssetSegment

pathExcerpt :: PathParser (Id Slot, Id Asset)
pathExcerpt = pathJSON >/> pathSlotId </> pathId </< "excerpt"

data CreateOrUpdateExcerptRequest =
    CreateOrUpdateExcerptRequest (Maybe Release)

postExcerpt :: ActionRoute (Id Slot, Id Asset)
postExcerpt = action POST pathExcerpt $ \(si, ai) -> withAuth $ do
  as <- getAssetSegment False PermissionEDIT False Nothing si ai
  e <- runForm Nothing $ do
    csrfForm
    CreateOrUpdateExcerptRequest rel <- CreateOrUpdateExcerptRequest <$> ("release" .:> deformNonEmpty deform)
    pure (Excerpt as rel)
  r <- changeExcerpt e
  unless r $ result $
    response conflict409 [] ("The requested excerpt overlaps an existing excerpt." :: T.Text)
  let notice t = createVolumeNotification (view e) $ \n -> (n t)
        { notificationContainerId = Just $ view e
        , notificationSegment = Just $ view e
        , notificationAssetId = Just $ view e
        , notificationRelease = excerptRelease e
        }
  when (isNothing $ assetExcerpt as) $
    notice NoticeExcerptVolume
  when (any ((effRelPublic . getAssetSegmentRelease2) as <) $ excerptRelease e) $
    notice NoticeReleaseExcerpt
  return $ okResponse [] $ JSON.pairs $ assetSegmentJSON (if r then as{ assetExcerpt = Just e } else as)

deleteExcerpt :: ActionRoute (Id Slot, Id Asset)
deleteExcerpt = action DELETE pathExcerpt $ \(si, ai) -> withAuth $ do
  guardVerfHeader
  as <- getAssetSegment False PermissionEDIT False Nothing si ai
  r <- removeExcerpt as
  return $ okResponse [] $ JSON.pairs $ assetSegmentJSON (if r then as{ assetExcerpt = Nothing } else as)
