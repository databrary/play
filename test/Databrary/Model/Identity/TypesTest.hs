{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Identity.TypesTest where

import Hedgehog
import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
import Test.Tasty.HUnit

import Databrary.Model.Identity.Types
import Databrary.Model.Party.TypesTest
-- import Databrary.Model.Token.TypesTest

unit_foldIdentity :: Assertion
unit_foldIdentity = do
    -- example
    runFoldIdentity NotLoggedIn @?= Default
    -- typical
    runFoldIdentity IdentityNotNeeded @?= Default
    runFoldIdentity (ReIdentified undefined) @?= Default
    runFoldIdentity (Identified undefined) @?= Extracted

runFoldIdentity :: Identity -> Result
runFoldIdentity = foldIdentity Default (const Extracted)

data Result = Default | Extracted deriving (Eq, Show)

unit_identitySuperuser :: Assertion
unit_identitySuperuser = do
    -- typical
    identitySuperuser (ReIdentified undefined) @? "reidentified is superuser"
    -- why True? is this because transcoding needs higher privileges to update asset?

genInitialIdentNeedAuthRoutes :: Gen Identity
genInitialIdentNeedAuthRoutes =
    Gen.choice
        [ pure NotLoggedIn
        , Identified <$> undefined -- TODO: finish gen session in Token types
        ]

genInitialIdentOpenRoutes :: Gen Identity
genInitialIdentOpenRoutes =
    pure IdentityNotNeeded

genReIdentified :: Gen Identity
genReIdentified =
    ReIdentified <$> genSiteAuthSimple -- TODO: come up with a better site auth generator
