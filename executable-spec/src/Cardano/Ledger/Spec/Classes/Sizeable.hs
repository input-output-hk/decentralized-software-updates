{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Spec.Classes.Sizeable where

import Data.Kind (Type)
import GHC.Exts (Constraint)


class (Num (Size s), Ord (Size s)) => Sizeable s where
  data Size s :: Type

  type HasSize s :: * -> Constraint

  size :: HasSize s a => a -> Size s
