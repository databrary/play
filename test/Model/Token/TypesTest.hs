{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Token.TypesTest where

import Data.Time
-- import Test.Tasty.HUnit

import Model.Token.Types
import Model.Id.Types
-- import Model.Party.TypesTest

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
