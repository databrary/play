{-# LANGUAGE OverloadedStrings #-}
module Controller.Citation
  ( getCitationHandler
  ) where

import Network.URI (URI)

import Has (focusIO)
import qualified JSON
import HTTP.Form.Deform
import Action
import Controller.Form
import Model.Citation.CrossRef

data GetCitationRequest = GetCitationRequest URI

getCitationHandler :: Action -- TODO: GET only
getCitationHandler = withoutAuth $ do
  GetCitationRequest url <- runForm Nothing $ (GetCitationRequest <$> ("url" .:> deform))
  cite <- maybeAction =<< focusIO (lookupCitation url)
  return $ okResponse [] $ JSON.toEncoding cite
