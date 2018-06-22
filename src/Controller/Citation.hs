{-# LANGUAGE OverloadedStrings #-}
module Controller.Citation
  ( getCitationHandler
  ) where

import Has (focusIO)
import qualified JSON as JSON
import HTTP.Form.Deform
import Action
import Controller.Form
import Model.Citation.CrossRef

getCitationHandler :: Action -- TODO: GET only
getCitationHandler = withoutAuth $ do
  url <- runForm Nothing $ "url" .:> deform
  cite <- maybeAction =<< focusIO (lookupCitation url)
  return $ okResponse [] $ JSON.toEncoding cite
