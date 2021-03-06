{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Identity.TypesTest where

-- import Hedgehog
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
import Test.Tasty.HUnit

import Model.Identity.Types
-- import Model.Party.TypesTest
-- import Model.Token.TypesTest

unit_extractFromIdentifiedSessOrDefault :: Assertion
unit_extractFromIdentifiedSessOrDefault = do
    -- example
    runExtractFromIdentifiedSessOrDefault NotLoggedIn @?= Default
    -- typical
    runExtractFromIdentifiedSessOrDefault IdentityNotNeeded @?= Default
    runExtractFromIdentifiedSessOrDefault (ReIdentified undefined) @?= Default
    runExtractFromIdentifiedSessOrDefault (Identified undefined) @?= Extracted

runExtractFromIdentifiedSessOrDefault :: Identity -> Result
runExtractFromIdentifiedSessOrDefault = extractFromIdentifiedSessOrDefault Default (const Extracted)

data Result = Default | Extracted deriving (Eq, Show)

unit_identitySuperuser :: Assertion
unit_identitySuperuser = do
    -- typical
    identitySuperuser (ReIdentified undefined) @? "reidentified is superuser"
    -- why True? is this because transcoding needs higher privileges to update asset?
