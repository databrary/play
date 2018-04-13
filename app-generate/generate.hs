import Databrary.Web.Rules

-- This file generates webfiles isolated from Databrary/Main.hs 
-- in to help with hot reloading of frontend web assets to 
-- compliment the frontend developer's environment. 

main :: IO ()
main = generateWebFilesNoStatic
