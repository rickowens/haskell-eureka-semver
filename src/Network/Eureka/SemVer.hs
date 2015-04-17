module Network.Eureka.SemVer (
    SemVer
  , addSemVer
  , parseSemVer
  ) where

import Data.Maybe                   (listToMaybe)
import Data.Version                 (Version, parseVersion, showVersion)
import Network.Eureka               (InstanceConfig (..), addMetadata)
import Text.ParserCombinators.ReadP (readP_to_S)

type SemVer = Version

metadataSemVerKey = "semver"

showSemVer :: SemVer -> String
showSemVer = showVersion

parseSemVer :: String -> Maybe SemVer
parseSemVer =
  fmap fst
  . listToMaybe
  . filter (null . snd)
  . readP_to_S parseVersion

addSemVer
  :: Version
  -> InstanceConfig
  -> InstanceConfig
addSemVer semver = addMetadata (metadataSemVerKey, showVersion semver)
