{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving
   , TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module Databrary.Model.VolumeTest where

import Control.Monad.Trans.Reader
import Data.Time
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure

import Databrary.Has
import Databrary.JSON
import Databrary.Model.Id
import Databrary.Model.Identity
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Service.DB

unit_getVolumeAlias :: Assertion
unit_getVolumeAlias =
    -- example
    getVolumeAlias blankVolume @?= Nothing
    -- typical
    -- edge cases

test_findVolumes :: TestTree
test_findVolumes = ignoreTest -- Because "??"
    (testCase "" _unit_findVolumes)

_unit_findVolumes :: Assertion
_unit_findVolumes = do
    let ident = PreIdentified
    cn <- loadPGDatabase >>= pgConnect
    let ctxt = Context cn ident
    vs <- runReaderT (findVolumes volumeFilter1 :: ReaderT Context IO [Volume]) ctxt  
    length vs @?= 2

volumeFilter1 :: VolumeFilter
volumeFilter1 =
    mempty

-- TODO: copied from Party, generalize

instance Has DBConn Context where
    view = ctxConn

instance Has Identity Context where
    view = ctxIdentity

instance Has SiteAuth Context where
    view = undefined

instance Has Party Context where
    view = undefined

instance Has (Id Party) Context where
    view = undefined

instance Has Access Context where
    view = undefined

data Context = Context
    { ctxConn :: DBConn
    , ctxIdentity :: Identity
    }

unit_volumeJSON :: Assertion
unit_volumeJSON = do
    (recordObject . volumeJSONSimple) volumeExample
        @?=
       ([("id",Number 1.0)
        ,("name",String "Test Vol One: A Survey")
        ,("body",String "Here is a description for a volume")
        ,("creation",String "2018-01-02T00:00:00Z")
        ,("owners",Array (V.fromList []))
        ,("permission",Number 1.0)
        ,("publicsharefull",Bool True)] :: [Pair])

volumeExample :: Volume
volumeExample =
    let
        row = 
           VolumeRow {
                 volumeId = Id 1
               , volumeName = "Test Vol One: A Survey"
               , volumeBody = Just "Here is a description for a volume"
               , volumeAlias = Just "Test Vol 1"
               , volumeDOI = Nothing
               }
    in
        Volume {
              volumeRow = row
            , volumeCreation = UTCTime (fromGregorian 2018 1 2) (secondsToDiffTime 0)
            , volumeOwners = []
            , volumePermission = PermissionPUBLIC
            , volumeAccessPolicy = PermLevelDefault
            }
