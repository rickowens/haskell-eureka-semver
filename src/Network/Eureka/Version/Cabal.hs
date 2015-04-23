module Network.Eureka.Version.Cabal (
    fromVersions
  , fromVersion
  , fromString
  , showVersion
  , versionInRangeP
  ) where

import Data.Maybe (listToMaybe)
import qualified Data.Version as DV (Version, Version (Version),
  parseVersion, showVersion)
import Network.Eureka.Version.Types (Version)
import Text.ParserCombinators.ReadP (readP_to_S)
import Network.Eureka.Version.Types (Predicate)

versionInRangeP :: [Int] -> [Int] -> Predicate
versionInRangeP lower upper ver =
      ver >= fromVersions lower
  &&  ver <  fromVersions upper

fromVersions :: [Int] -> Version
fromVersions vs = fromVersion $ DV.Version vs []

fromVersion :: DV.Version -> Version
fromVersion = id

fromString :: String -> Maybe Version
fromString =
  fmap fst
  . listToMaybe
  . filter (null . snd)
  . readP_to_S DV.parseVersion

showVersion :: Version -> String
showVersion = DV.showVersion

