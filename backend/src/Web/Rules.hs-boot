module Web.Rules where

import Web (WebFilePath)
import Web.Types (WebGeneratorM, WebFileInfo)

generateWebFile :: Bool -> WebFilePath -> WebGeneratorM WebFileInfo
