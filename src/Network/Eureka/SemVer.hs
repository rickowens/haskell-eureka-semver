module Network.Eureka.SemVer (
    addSemVer
  ) where

import           Data.Version   (Version, showVersion)
import           Network.Eureka (InstanceConfig (..), addMetadata)

addSemVer
  :: Version
  -> InstanceConfig
  -> InstanceConfig
addSemVer semver = addMetadata ("semver", showVersion semver)
