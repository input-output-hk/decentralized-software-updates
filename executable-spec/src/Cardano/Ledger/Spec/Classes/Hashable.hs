{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Spec.Classes.Hashable where

import Data.Kind (Type)
import GHC.Exts (Constraint)


class ( forall a . Eq (Hash h a)
      , forall a . Ord (Hash h a)
      , forall a . Show (Hash h a)
      ) => Hashable h where

  data Hash h :: * -> Type

  type HasHash h :: * -> Constraint

  hash :: HasHash h a => a -> Hash h a
