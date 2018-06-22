{-# LANGUAGE TemplateHaskell #-}
module Model.Permission.SQL
  ( accessRow
  , accessSets
  ) where

import Model.SQL.Select
import Model.Permission.Types

accessRow :: String -- ^ Table name
  -> Selector -- ^ 'Access'
accessRow table = selectColumns 'Access table ["site", "member"]

accessSets :: String -- ^ @'Access'@
  -> [(String, String)]
accessSets a =
  [ ("site", "${accessSite " ++ a ++ "}")
  , ("member", "${accessMember " ++ a ++ "}")
  ]
