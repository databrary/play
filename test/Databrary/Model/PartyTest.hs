{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.PartyTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Party

unit_partyName_example :: Assertion
unit_partyName_example = do
    (partyName . partyRow) nobodyParty @?= "Everybody"
