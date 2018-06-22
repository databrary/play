{-# LANGUAGE OverloadedStrings #-}
module Controller.Comment
  ( postComment
  ) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.Class (lift)
import Data.Function (on)

import Ops
import Has
import qualified JSON as JSON
import Model.Permission
import Model.Id
import Model.Container
import Model.Slot
import Model.Notification.Types
import Model.Party.Types
import Model.Comment
import HTTP.Form.Deform
import HTTP.Path.Parser
import Action
import Controller.Paths
import Controller.Permission
import Controller.Form
import Controller.Slot
import Controller.Notification
import View.Form (FormHtml)
-- import View.Comment

postComment :: ActionRoute (Id Slot)
postComment = action POST (pathJSON >/> pathSlotId </< "comment") $ \si -> withAuth $ do
  u <- authAccount
  s <- getSlot PermissionSHARED Nothing si
  (c, p) <- runForm (Nothing :: Maybe (RequestContext -> FormHtml a)) $ do
    csrfForm
    text <- "text" .:> (deformRequired =<< deform)
    parent <- "parent" .:> deformNonEmpty (deformMaybe' "comment not found" =<< lift . lookupComment =<< deform)
    return ((blankComment u s)
      { commentText = text
      , commentParents = maybe [] (return . commentId) parent
      }, parent)
  c' <- addComment c
  top <- containerIsVolumeTop (slotContainer s)
  forM_ p $ \r -> when (on (/=) (partyId . partyRow . accountParty) (commentWho r) u) $
    createNotification (blankNotification (commentWho r) NoticeCommentReply)
      { notificationContainerId = top `unlessUse` (view c')
      , notificationSegment = Just $ view c'
      , notificationCommentId = Just $ view c'
      }
  createVolumeNotification (view c') $ \n -> (n NoticeCommentVolume)
    { notificationContainerId = top `unlessUse` (view c')
    , notificationSegment = Just $ view c'
    , notificationCommentId = Just $ view c'
    }
  return $ okResponse [] $ JSON.recordEncoding $ commentJSON c'
  -- HTML -> peeks $ otherRouteResponse [] (viewSlot False) (api, (Just (view c'), slotId (commentSlot c')))
