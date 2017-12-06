{-# LANGUAGE OverloadedStrings, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.JSONHtml where

import Data.Aeson

import qualified Text.Blaze.Html as Html
import qualified Text.Blaze.Html.Renderer.Text as Html

instance ToJSON Html.Html where
  toJSON = toJSON . Html.renderHtml
  toEncoding = toEncoding . Html.renderHtml

