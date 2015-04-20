module Network.Eureka.Version (
    Version
  , addVersion
  , lookupVersion
  ) where

import Network.Eureka               (InstanceConfig (..), addMetadata,
                                     lookupMetadata)
import Network.Eureka.Version.Cabal (showVersion)
import Network.Eureka.Version.Types (Version)

metadataVersionKey :: String
metadataVersionKey = "version"

addVersion
  :: Version
  -> InstanceConfig
  -> InstanceConfig
addVersion version = addMetadata (metadataVersionKey, showVersion version)
  where

lookupVersion :: InstanceConfig -> Maybe String
lookupVersion = lookupMetadata metadataVersionKey
