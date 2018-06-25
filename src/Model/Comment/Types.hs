{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Model.Comment.Types
  ( Comment(..)
  , CommentRow(..)
  , makeComment
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Has (Has(..))
import Model.Container.Types
import Model.Kind
import Model.Time
import Model.Id.Types
import Model.Party.Types
import Model.Segment
import Model.Slot.Types
import Model.Volume.Types

type instance IdType Comment = Int32

data Comment = Comment
  { commentId :: Id Comment
  , commentWho :: Account
  , commentSlot :: Slot
  , commentTime :: Timestamp
  , commentText :: T.Text
  , commentParents :: [Id Comment]
  }

instance Kinded Comment where
  kindOf _ = "comment"

instance Has (Id Comment) Comment where
  view = commentId
instance Has Model.Segment.Segment Comment where
  view = (view . commentSlot)
instance Has (Id Model.Container.Types.Container) Comment where
  view = (view . commentSlot)
instance Has Model.Volume.Types.Volume Comment where
  view = (view . commentSlot)
{- instance Has (Id Model.Volume.Types.Volume) Comment where
  view = (view . commentSlot) -}

data CommentRow = CommentRow
  { commentRowId :: Id Comment
  , commentRowWhoId :: Id Party
  , commentRowSlotId :: SlotId
  , commentRowTime :: Timestamp
  , commentRowText :: T.Text
  }

makeComment :: Id Comment -> Segment -> Timestamp -> T.Text -> [Maybe (Id Comment)] -> Account -> Container -> Comment
makeComment i s t x p w c = Comment i w (Slot c s) t x (map (fromMaybe (error "NULL comment thread")) p)
