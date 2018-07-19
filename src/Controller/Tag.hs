{-# LANGUAGE OverloadedStrings #-}
module Controller.Tag
  ( queryTags
  , postTag
  , deleteTag
  ) where

import qualified Data.Aeson as Aeson
import Control.Monad (unless)
import qualified Data.Text as T
import Network.HTTP.Types (conflict409)
import qualified Web.Route.Invertible as R

import Has
import Ops
import qualified JSON as JSON
import Model.Permission
import Model.Id
import Model.Container
import Model.Slot
import Model.Tag
import Model.Notification.Types
import Solr.Tag
-- import HTTP.Form.Deform
import HTTP.Path.Parser
import Action.Run
import Action
import Controller.Paths
-- import Controller.Form
import Controller.Permission
import Controller.Slot
import Controller.Notification

-- TODO: use validateTag after receiving string in url first, similar to below
-- _tagNameForm :: DeformHandler f TagName
-- _tagNameForm = deformMaybe' "Invalid tag name." . validateTag =<< deform

queryTags :: ActionRoute (Maybe TagName)
queryTags = action GET (pathJSON >/> "tags" >/> pathMaybe R.parameter) $ \t -> withoutAuth $
  okResponse [] . Aeson.encode . unwrap . QueryTagsResponse <$> termTags t 16

-- | Response body for queryTags
newtype QueryTagsResponse = QueryTagsResponse { unwrap :: [TagName] }

tagResponse :: API -> TagUse -> Handler Response
tagResponse JSON t = okResponse [] . JSON.recordEncoding . tagCoverageJSON <$> lookupTagCoverage (useTag t) (containerSlot $ slotContainer $ tagSlot t)
tagResponse HTML t = peeks $ otherRouteResponse [] (viewSlot False) (HTML, (Just (view t), slotId (tagSlot t)))

postTag :: ActionRoute (API, Id Slot, TagId)
postTag = action POST (pathAPI </>> pathSlotId </> pathTagId) $ \(api, si, TagId kw tn) -> withAuth $ do
  guardVerfHeader
  u <- authAccount
  s <- getSlot (if kw then PermissionEDIT else PermissionSHARED) Nothing si
  t <- addTag tn
  let tu = TagUse t kw u s
  r <- addTagUse tu
  unless r $ result $
    response conflict409 [] ("The requested tag overlaps your existing tag." :: T.Text)
  top <- containerIsVolumeTop (slotContainer s)
  createVolumeNotification (view tu) $ \n -> (n NoticeTagVolume)
    { notificationContainerId = top `unlessUse` view tu
    , notificationSegment = Just $ (view . tagSlot) tu
    , notificationTag = Just $ useTag tu
    }
  tagResponse api tu

deleteTag :: ActionRoute (API, Id Slot, TagId)
deleteTag = action DELETE (pathAPI </>> pathSlotId </> pathTagId) $ \(api, si, TagId kw tn) -> withAuth $ do
  guardVerfHeader
  u <- authAccount
  s <- getSlot (if kw then PermissionEDIT else PermissionSHARED) Nothing si
  t <- maybeAction =<< lookupTag tn
  let tu = TagUse t kw u s
  _r <- removeTagUse tu
  tagResponse api tu
