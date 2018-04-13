module Databrary.Web.Rules where

import Databrary.Web (WebFilePath)
import Databrary.Web.Types (WebGeneratorM, WebFileInfo)

generateWebFileNoStatic :: WebFilePath -> WebGeneratorM WebFileInfo
