{-# LANGUAGE ScopedTypeVariables #-}
module Controller.Metric
  ( postVolumeMetric
  , deleteVolumeMetric
  ) where

import Control.Invertible.Monoidal ((>|<))

import qualified JSON as JSON
import Model.Id
import Model.Permission
import Model.Volume
import Model.Category
import Model.Metric
import Model.VolumeMetric
import HTTP.Path.Parser
import Action.Route
import Action
import Controller.Paths
import Controller.Volume

postVolumeMetric :: ActionRoute (Id Volume, Either (Id Category) (Id Metric))
postVolumeMetric = action PUT (pathJSON >/> pathId </> (pathId >|< pathId)) $ \(vi, cm) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  (addedMetrics :: [Id Metric]) <-
      either
          (\catId -> addVolumeCategory v catId)
          (\metricId' -> do
              metricAdded <- addVolumeMetric v metricId'
              return $ if metricAdded then [metricId'] else [])
          cm
  return $ okResponse [] $ JSON.toEncoding addedMetrics

deleteVolumeMetric :: ActionRoute (Id Volume, Either (Id Category) (Id Metric))
deleteVolumeMetric = action DELETE (pathJSON >/> pathId </> (pathId >|< pathId)) $ \(vi, cm) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  r <- either (removeVolumeCategory v) (fmap fromEnum . removeVolumeMetric v) cm
  return $ okResponse [] $ JSON.toEncoding r
