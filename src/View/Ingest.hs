{-# LANGUAGE OverloadedStrings #-}
module View.Ingest
  ( htmlIngestForm
  ) where

import Control.Monad (void, forM_)
import qualified Data.Aeson as JSON
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H

import Action.Types
import Action.Route
import Model.Id.Types
import Model.Volume
import Ingest.Service
import View.Html
import View.Form

import Controller.Angular
import Controller.Container
import {-# SOURCE #-} Controller.Ingest

htmlIngestStatus :: IngestStatus -> JSOpt -> H.Html
htmlIngestStatus IngestInactive _ = mempty
htmlIngestStatus (IngestActive _) _ = void "An ingest is currently running..."
htmlIngestStatus (IngestFailed el) _ = do
  void "Previous ingest failed:"
  H.ul $ mapM_ (H.li . H.text) el
htmlIngestStatus (IngestCompleted cl) js = do
  void "Previous ingest completed:"
  H.ul $ forM_ cl $ \c ->
    H.li $ H.a H.! actionLink viewContainer (HTML, (Nothing, Id c)) js $ H.toMarkup c

htmlIngestForm :: Volume -> IngestStatus -> RequestContext -> FormHtml JSON.Value
htmlIngestForm v s = htmlForm
  ("Ingest " <> volumeName (volumeRow v))
  postIngest (volumeId $ volumeRow v)
  (case s of
    IngestActive _ ->
      field "abort" $ inputCheckbox False
    _ -> do
      field "run" $ inputCheckbox False
      field "overwrite" $ inputCheckbox False
      field "json" inputFile)
  (htmlIngestStatus s)
