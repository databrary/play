{-# LANGUAGE OverloadedStrings #-}
module View.Error
  ( htmlNotFound
  , htmlForbidden
  ) where

import Control.Monad (void)
import qualified Text.Blaze.Html5 as H

import Action.Types
import View.Template

htmlNotFound :: RequestContext -> H.Html
htmlNotFound req = htmlTemplate req (Just "Not found") $ \_ ->
  void $ "The resource you requested may no longer be available."

htmlForbidden :: RequestContext -> H.Html
htmlForbidden req = htmlTemplate req (Just "Access denied") $ \_ ->
  void $ "You do not have access to the requested resource."
