{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Citation
  ( getCitationHandler
  ) where

import Databrary.Has (focusIO)
import qualified Databrary.JSON as JSON
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Form
import Databrary.Model.Citation.CrossRef

getCitationHandler :: Action -- TODO: GET only
getCitationHandler = withoutAuth $ do
  url <- runForm Nothing $ "url" .:> deform
  cite <- maybeAction =<< focusIO (lookupCitation url)
  return $ okResponse [] $ JSON.toEncoding cite
