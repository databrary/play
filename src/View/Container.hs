{-# LANGUAGE OverloadedStrings #-}
module View.Container
  ( releaseTitle
  --, htmlContainerEdit
  ) where

import qualified Data.ByteString.Char8 as BSC
-- import Data.Foldable (fold)
-- import Data.Monoid ((<>))
import qualified Data.Text as T

import qualified Store.Config as C
-- import Model.Volume.Types
-- import Model.Container.Types
-- import Model.Slot.Types
import Model.Release.Types
import Service.Messages
-- import Action.Types
-- import Action
-- import View.Form

--import {-# SOURCE #-} Controller.Container

releaseTitle :: Maybe Release -> Messages -> T.Text
releaseTitle rel = getMessage $ C.Path ["release", maybe "UNRELEASED" (BSC.pack . show) rel, "title"]

{-
htmlContainerForm :: Maybe Container -> FormHtml f
htmlContainerForm cont = do
  field "name" $ inputText (containerName . containerRow =<< cont)
  field "date" $ inputDate (containerDate . containerRow =<< cont)
  field "release" $ inputEnum False (containerRelease =<< cont)

htmlContainerEdit :: Either Volume Container -> RequestContext -> FormHtml f
htmlContainerEdit (Left v)  = htmlForm "Create container" createContainer (volumeId $ volumeRow v) (htmlContainerForm Nothing) (const mempty)
htmlContainerEdit (Right c) = htmlForm ("Edit container " <> fold (containerName $ containerRow c)) postContainer (containerSlotId $ containerId $ containerRow c) (htmlContainerForm $ Just c) (const mempty)
-}
