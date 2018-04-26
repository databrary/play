{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving
   , TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
module Databrary.Model.VolumeTest where

import Control.Monad.Trans.Reader
import Test.Tasty.HUnit

import Databrary.Has
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

unit_findVolumes :: Assertion
unit_findVolumes = do
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
