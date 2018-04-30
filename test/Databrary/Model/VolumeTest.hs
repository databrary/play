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

unit_volumeJSONSimple_example :: Assertion
unit_volumeJSONSimple_example = do
    (recordObject . volumeJSONSimple) volumeExample
        @?=
       ([("id",Number 1.0)
        ,("name",String "Test Vol One: A Survey")
        ,("body",String "Here is a description for a volume")
        ,("creation",String "2018-01-02T00:00:00Z")
        ,("owners",Array (V.fromList []))-- (V.fromList [("name",String "Smith, John"),("id",Number 2.0)]))
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
            , volumeOwners = [] -- [(Id 2, "Smith, John")]
            , volumePermission = PermissionPUBLIC
            , volumeAccessPolicy = PermLevelDefault
            }

unit_lookupVolume_example :: Assertion
unit_lookupVolume_example = do
    cn <- loadPGDatabase >>= pgConnect
    let ident = PreIdentified
    mVol <- runReaderT (lookupVolume (Id 1)) (Context cn ident)
    mVol @?=
       Just
        (Volume
                 { volumeRow = VolumeRow
                               { volumeId = Id 1
                               , volumeName = "Databrary"
                               , volumeBody = Just
                                                 "Databrary is an open data library for developmental science. Share video, audio, and related metadata. Discover more, faster.\nMost developmental scientists rely on video recordings to capture the complexity and richness of behavior. However, researchers rarely share video data, and this has impeded scientific progress. By creating the cyber-infrastructure and community to enable open video sharing, the Databrary project aims to facilitate deeper, richer, and broader understanding of behavior.\nThe Databrary project is dedicated to transforming the culture of developmental science by building a community of researchers committed to open video data sharing, training a new generation of developmental scientists and empowering them with an unprecedented set of tools for discovery, and raising the profile of behavioral science by bolstering interest in and support for scientific research among the general public."
                               , volumeAlias = Nothing
                               , volumeDOI = Just "10.17910/B7159Q"
                               }
                 , volumeCreation = UTCTime (fromGregorian 2013 1 11) (secondsToDiffTime 37600)
                 , volumeOwners = [ (Id 1, "Admin, Admin")
                                  , (Id 3, "Steiger, Lisa")
                                  , (Id 7, "Tesla, Testarosa")
                                  ]
                 , volumePermission = PermissionPUBLIC
                 , volumeAccessPolicy = PermLevelDefault
                 }
        )
