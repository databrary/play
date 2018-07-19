{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Controller.Search
  ( postSearchHandler
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Has
import Model.Id.Types
import Model.Metric
import Solr.Search
import Action.Response
import Action
import HTTP.Form (FormKey(..))
import HTTP.Form.Deform
import Controller.Form
import Controller.Angular

searchForm :: DeformHandler f SearchQuery
searchForm = SearchQuery
  <$> ("q" .:> deformNonEmpty deform)
  <*> ("f" .:> withSubDeforms (\k -> (view k, ) <$> deform))
  <*> ("m" .:> withSubDeforms (\k -> (,)
    <$> either deformError' return (maybe (Left "Metric ID not found") Right . getMetric . Id =<< case k of
      FormField t -> textInteger t
      FormIndex i -> Right (fromIntegral i))
    <*> deform))
  <*> ("volume" .:> fromMaybe SearchVolumes <$> deformOptional (sv <$> deform))
  <*> paginateForm
  where
  sv False = SearchParties
  sv True = SearchVolumes

postSearchHandler :: API -> Action  -- TODO: GET only
postSearchHandler = \api -> withAuth $ do
  when (api == HTML) angular
  q <- runForm Nothing searchForm
  proxyResponse <$> search q
