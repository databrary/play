{-# LANGUAGE TemplateHaskell, DataKinds #-}
module Databrary.Model.Format.Boot
  ( loadFormats
  ) where

import Database.PostgreSQL.Typed.Array ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.SQL.Select
import Databrary.Model.Format.SQL

loadFormats :: TH.ExpQ
loadFormats = do
  l <- runTDB 
         $ dbQuery 
            $(selectQuery 
                (selectColumns 'makeFormat "format" ["id", "mimetype", "extension", "name"]) 
                "ORDER BY id")
  TH.lift l
