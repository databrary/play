{-# LANGUAGE OverloadedStrings #-}
module Action.Request
  ( isDatabraryClient
  ) where

import qualified Network.Wai as Wai

import HTTP.Request

isDatabraryClient :: Wai.Request -> Bool
isDatabraryClient = any ("DatabraryClient" ==) . lookupRequestHeader "x-requested-with"
