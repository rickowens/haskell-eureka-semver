module Network.Eureka.Version.Cabal (
    fromVersion
  , fromString
  , showVersion
  ) where

import           Data.Maybe                   (listToMaybe)
import qualified Data.Version                 as DV (Version, parseVersion,
                                                     showVersion)
import           Network.Eureka.Version.Types (Version)
import           Text.ParserCombinators.ReadP (readP_to_S)


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

