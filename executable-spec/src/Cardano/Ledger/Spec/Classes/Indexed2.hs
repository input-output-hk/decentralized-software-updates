{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Spec.Classes.Indexed2 where

import           Prelude hiding (lookup)

import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

class Indexed m where
  type Key m :: *

  type Value m :: *

  lookup :: Key m -> m -> Maybe (Value m)

(!) :: Indexed m => m -> Key m -> Maybe (Value m)
(!) = flip lookup

instance Ord k => Indexed (Map k v) where
  type Key (Map k v) = k

  type Value (Map k v) = v

  lookup = Map.lookup

instance (Ord k, Ord v) => Indexed (Bimap k v) where
  type Key (Bimap k v) = k

  type Value (Bimap k v) = v

  lookup = Bimap.lookup

withValue :: Applicative m => Maybe a -> b -> (a -> m b) -> m b
withValue Nothing  b _ = pure b
withValue (Just a) _ f = f a
