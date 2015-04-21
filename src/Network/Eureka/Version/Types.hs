module Network.Eureka.Version.Types (Version, Predicate) where

import qualified Data.Version as DV (Version)

type Version = DV.Version
type Predicate = Version -> Bool

