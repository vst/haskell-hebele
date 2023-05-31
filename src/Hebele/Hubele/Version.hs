-- | This module provides application version related definitions.
module Hebele.Hubele.Version where

import qualified Data.Text as T
import Data.Version (Version, showVersion)
import qualified Paths_hebele_hubele as Paths


-- | Application version.
--
-- > version
-- Version {versionBranch = [0,0,0], versionTags = []}
version :: Version
version = Paths.version


-- | Application version as a 'String' value.
--
-- > versionString
-- "0.0.0"
versionString :: String
versionString = showVersion version


-- | Application version as a 'Data.Text.Text' value.
--
-- > versionText
-- "0.0.0"
versionText :: T.Text
versionText = T.pack versionString
