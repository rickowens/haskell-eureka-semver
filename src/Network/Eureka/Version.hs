module Network.Eureka.Version (
    Version,
    Predicate,
    addVersion,
    lookupVersion,
    filterInstancesWithPredicate,
    versionFilter',
    versionFilter,
    range,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (join)
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


{- |
  Like `versionFilter'`, but if an `InstanceInfo` doesn't have a version, or
  the version is not parsable, then the resulting predicate function returns
  `False`.
-}
versionFilter :: Predicate -> InstanceInfo -> Bool
versionFilter = versionFilter' False


{- |
  Accept a `Predicate` and convert it into a predicate function suitable
  for filtering `InstanceInfo`s using `Prelude.filter`.

  e.g.

  > let infos :: [InstanceInfo]
  >     pred :: Predicate
  > in filter (versionFilter pred) infos :: [InstanceInfo]

  If an `InstanceInfo` doesn't have a version, or the version is not parsable,
  then the resulting predicate function returns `defaultResult`.
-}
versionFilter' :: Bool -> Predicate -> InstanceInfo -> Bool
versionFilter' defaultResult predicate =
  maybe defaultResult predicate . join . fmap VC.fromString . lookupVersion


{- |
  Build a filter that accepts a range of compatible versions, in the
  tradition of semver. Matching versions will include versions that
  are greater than or equal to minVersion, and that are strictly less
  than the max versions. That is to say, minVersion is inclusive, while
  maxVersion exclusive.
-}
range
  :: Version
    -- @minVersion@ - The minimum allowed version, inclusive.
  -> Version
    -- @maxVersion@ - The maximum allowed version, exclusive.
  -> InstanceInfo
  -> Bool
range minVersion maxVersion =
  versionFilter (\version -> version >= minVersion && version < maxVersion)


filterInstancesWithPredicate
  :: Predicate
  -> [InstanceInfo]
  -> [InstanceInfo]
filterInstancesWithPredicate predicate =
  filter predicateWithFailures
    where
      predicateWithFailures :: InstanceInfo -> Bool
      predicateWithFailures instanceInfo =
        case VC.fromString <$> lookupVersion instanceInfo of
          Nothing -> error
            $ "instance with the following InstanceInfo does not "
            ++ "contain version: " ++ show instanceInfo
          Just Nothing -> error
            $ "the version could not be parsed for the instance with "
            ++ "the following InstanceInfo: " ++ show instanceInfo
          Just (Just version) ->
            predicate version


