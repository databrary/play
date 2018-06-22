{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Permission.TypesTest where

import Model.Permission.Types

{- PermissionNONE | PermissionPUBLIC | PermissionSHARED | PermissionREAD | PermissionEDIT |
   PermissionADMIN -}

accessCommunity :: Access -- correct?
accessCommunity = Access { accessSite' = PermissionSHARED, accessMember' = PermissionSHARED }

accessPublic :: Access -- correct?
accessPublic = Access { accessSite' = PermissionPUBLIC, accessMember' = PermissionPUBLIC }

accessNone :: Access -- correct?
accessNone = Access { accessSite' = PermissionNONE, accessMember' = PermissionNONE }
