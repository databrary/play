{-# ScopedTypeVariables #-}
module Databrary.Controller.Metric
  ( postVolumeMetric
  , deleteVolumeMetric
  ) where

import Control.Invertible.Monoidal ((>|<))

import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Category
import Databrary.Model.Metric
import Databrary.Model.VolumeMetric
import Databrary.HTTP.Path.Parser
import Databrary.Action.Route
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Volume

postVolumeMetric :: ActionRoute (Id Volume, Either (Id Category) (Id Metric))
postVolumeMetric = action PUT (pathJSON >/> pathId </> (pathId >|< pathId)) $ \(vi, cm) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  (addedMetrics :: [Id Metric]) <-
      either
          (\categoryId -> addVolumeCategory v categoryId)
          (\metricId -> do
              metricAdded <- addVolumeMetric v metricId
              return $ if metricAdded then [metricId] else [])
          cm
  return $ okResponse [] $ JSON.toEncoding addedMetrics

deleteVolumeMetric :: ActionRoute (Id Volume, Either (Id Category) (Id Metric))
deleteVolumeMetric = action DELETE (pathJSON >/> pathId </> (pathId >|< pathId)) $ \(vi, cm) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  r <- either (removeVolumeCategory v) (fmap fromEnum . removeVolumeMetric v) cm
  return $ okResponse [] $ JSON.toEncoding r
