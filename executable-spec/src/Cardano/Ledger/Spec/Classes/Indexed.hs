{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Spec.Classes.Indexed where

import           Prelude hiding (lookup)

import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

class Indexed m where
  type Key m :: *

  type Value m :: *

  lookup :: Key m -> m -> Value m

(!) :: Indexed m => m -> Key m -> Value m
(!) = flip lookup

instance Ord k => Indexed (Map k v) where
  type Key (Map k v) = k

  type Value (Map k v) = v

  lookup = flip (Map.!)

instance (Ord k, Ord v) => Indexed (Bimap k v) where
  type Key (Bimap k v) = k

  type Value (Bimap k v) = v

  lookup = flip (Bimap.!)
