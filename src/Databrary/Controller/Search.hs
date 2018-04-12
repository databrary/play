{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Databrary.Controller.Search
  ( postSearchHandler
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Databrary.Has
import Databrary.Model.Id.Types
import Databrary.Model.Metric
import Databrary.Solr.Search
import Databrary.Action.Response
import Databrary.Action
import Databrary.HTTP.Form (FormKey(..))
import Databrary.HTTP.Form.Deform
import Databrary.Controller.Form
import Databrary.Controller.Angular

searchForm :: DeformActionM f SearchQuery
searchForm = SearchQuery
  <$> ("q" .:> deformNonEmpty deform)
  <*> ("f" .:> withSubDeforms (\k -> (view k, ) <$> deform))
  <*> ("m" .:> withSubDeforms (\k -> (,)
    <$> (either deformError' return $ maybe (Left "Metric ID not found") Right . getMetric . Id =<< case k of
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
