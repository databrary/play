{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Token.TypesTest where

import Data.Time
-- import Test.Tasty.HUnit

import Databrary.Model.Token.Types
import Databrary.Model.Id.Types
-- import Databrary.Model.Party.TypesTest

token1 :: Token
token1 =
    Token {
          tokenId = Id "tk1"
        , tokenExpires = UTCTime (fromGregorian 2017 1 2) (secondsToDiffTime 0)
        }

-- gen token

-- gen account token
--    -- token, siteauth - see elsewhere

-- gen session -- uses account token, see above
--    gen verf
--    gen superuser
