module Network.Eureka.Version (
    Version
  , Predicate
  , addVersion
  , lookupVersion
  , filterInstancesWithPredicate
  ) where

import Network.Eureka (InstanceConfig, InstanceInfo, addMetadata,
  lookupMetadata)
import Network.Eureka.Version.Types (Predicate, Version)
import qualified Network.Eureka.Version.Cabal as VC (fromString, showVersion)

metadataVersionKey :: String
metadataVersionKey = "version"

addVersion
  :: Version
  -> InstanceConfig
  -> InstanceConfig
addVersion version = addMetadata (metadataVersionKey, VC.showVersion version)

lookupVersion :: InstanceInfo -> Maybe String
lookupVersion = lookupMetadata metadataVersionKey


filterInstancesWithPredicate
  :: Predicate
  -> [InstanceInfo]
  -> [InstanceInfo]
filterInstancesWithPredicate predicate =
  filter predicateWithFailures
    where
      predicateWithFailures :: InstanceInfo -> Bool
      predicateWithFailures instanceInfo =
        case lookupVersion instanceInfo of
          Nothing             -> error $ "instance with the following InstanceInfo does not contain version: " ++ show instanceInfo
          Just versionString  ->
            case VC.fromString versionString of
              Nothing         -> error $ "the version could not be parsed for the instance with the following InstanceInfo: " ++ show instanceInfo
              Just version    -> predicate version


