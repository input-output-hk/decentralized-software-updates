{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Ledger.Spec.Classes.HasSigningScheme where

import Data.Kind (Type)
import GHC.Exts (Constraint)


class HasSigningScheme s where

  data Signature s :: * -> Type

  type VKey s :: Type

  type SKey s :: Type

  type Signable s :: * -> Constraint

  sign :: Signable s a => SKey s -> a -> Signature s a

  verify :: Signable s a => VKey s -> Signature s a -> a -> Bool
