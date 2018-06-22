{-# LANGUAGE OverloadedStrings #-}
module Controller.Funding
  ( queryFunderHandler
  , postVolumeFunding
  , deleteVolumeFunder
  ) where

import Control.Monad (liftM2)
import qualified Data.Text as T

import Has (focusIO)
import qualified JSON as JSON
import Model.Id
import Model.Permission
import Model.Volume
import Model.Funding
import Model.Funding.FundRef
import HTTP.Form.Deform
import HTTP.Path.Parser
import Action
import Controller.Paths
import Controller.Form
import Controller.Permission
import Controller.Volume

queryFunderHandler :: Action -- TODO: GET only
queryFunderHandler = withAuth $ do
  _ <- authAccount
  (q, a) <- runForm Nothing $ liftM2 (,)
    ("query" .:> (deformRequired =<< deform))
    ("all" .:> deform)
  r <- if a
    then focusIO $ searchFundRef q
    else findFunders q
  return $ okResponse [] $ JSON.mapObjects funderJSON r

postVolumeFunding :: ActionRoute (Id Volume, Id Funder)
postVolumeFunding = action POST (pathJSON >/> pathId </> pathId) $ \(vi, fi) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  f <- maybeAction =<< lookupFunderRef fi
  a <- runForm Nothing $ do
    csrfForm
    "awards" .:> filter (not . T.null) <$> withSubDeforms (\_ -> deform)
  let fa = Funding f a
  _ <- changeVolumeFunding v fa
  return $ okResponse [] $ JSON.pairs $ fundingJSON fa

deleteVolumeFunder :: ActionRoute (Id Volume, Id Funder)
deleteVolumeFunder = action DELETE (pathJSON >/> pathId </> pathId) $ \(vi, fi) -> withAuth $ do
  guardVerfHeader
  v <- getVolume PermissionEDIT vi
  _ <- removeVolumeFunder v fi
  return $ okResponse [] $ JSON.recordEncoding $ volumeJSONSimple v
