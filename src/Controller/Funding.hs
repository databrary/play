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
import Model.Volume hiding (getVolume)
import Model.Funding
import Model.Funding.FundRef
import HTTP.Form.Deform
import HTTP.Path.Parser
import Action
import Controller.Paths
import Controller.Form
import Controller.Permission
import Controller.Volume

data QueryFundersRequest = QueryFundersRequest T.Text Bool

queryFunderHandler :: Action -- TODO: GET only
queryFunderHandler = withAuth $ do
  _ <- authAccount
  QueryFundersRequest q a <- runForm Nothing $ liftM2 QueryFundersRequest
    ("query" .:> (deformRequired =<< deform))
    ("all" .:> deform)
  r <- QueryFunderResponse <$> if a
    then focusIO $ searchFundRef q
    else findFunders q
  return $ okResponse [] $ (JSON.mapObjects funderJSON . unwrap) r

-- | Body of funder query response
newtype QueryFunderResponse = QueryFunderResponse { unwrap :: [Funder] }

data CreateOrUpdateVolumeFundingRequest =
    CreateOrUpdateVolumeFundingRequest [T.Text]

postVolumeFunding :: ActionRoute (Id Volume, Id Funder)
postVolumeFunding = action POST (pathJSON >/> pathId </> pathId) $ \(vi, fi) -> withAuth $ do
  v <- getVolume PermissionEDIT vi
  f <- maybeAction =<< lookupFunderRef fi
  CreateOrUpdateVolumeFundingRequest a <- runForm Nothing $ do
    csrfForm
    CreateOrUpdateVolumeFundingRequest <$> ("awards" .:> filter (not . T.null) <$> withSubDeforms (\_ -> deform))
  let resp@(AddVolumeFundingResponse fa) = AddVolumeFundingResponse (Funding f a)
  _ <- changeVolumeFunding v fa
  return $ okResponse [] $ JSON.pairs $ (fundingJSON . avfUnwrap) resp

-- | Body of add volume funding response
newtype AddVolumeFundingResponse = AddVolumeFundingResponse { avfUnwrap :: Funding } 

deleteVolumeFunder :: ActionRoute (Id Volume, Id Funder)
deleteVolumeFunder = action DELETE (pathJSON >/> pathId </> pathId) $ \(vi, fi) -> withAuth $ do
  guardVerfHeader
  v <- getVolume PermissionEDIT vi
  _ <- removeVolumeFunder v fi
  return $ okResponse [] $ JSON.recordEncoding $ volumeJSONSimple v
