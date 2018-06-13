{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving
   , TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}
module Databrary.Model.FundingTest where

-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Reader
-- import qualified Data.ByteString as BS
-- import Data.Maybe
-- import qualified Data.Text as T
-- import Data.Time
-- import Hedgehog
-- import Hedgehog.Gen as Gen
-- import Test.Tasty
import Test.Tasty.HUnit

-- import Databrary.Has
-- import Databrary.Model.Id
-- import Databrary.Model.Identity
-- import Databrary.Model.Party
-- import Databrary.Model.Party.TypesTest
-- import Databrary.Model.Permission
-- import Databrary.Model.Token
import Databrary.Model.Funding
-- import Databrary.Service.DB
import TestHarness

-- session driving a variety of functions in the module
unit_Funding_examples :: Assertion
unit_Funding_examples =
    withinTestTransaction (\cn -> do
        let ctxt = TestContext { ctxConn = Just cn }
            fndr = Funder (Id 1) "The Big Fund"
        matches <-
            runReaderT
              (do
                  _ <- addFunder fndr
                  findFunders "Big")
              ctxt
        matches @?= [fndr])
