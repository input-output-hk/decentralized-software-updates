{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Ledger.Spec.Classes.HasSigningScheme where

import Data.Kind (Type)
import GHC.Exts (Constraint)


class ( forall a . Eq (Signature s a)
      , forall a . Show (Signature s a)
      , Show (VKey s)
      , Show (SKey s)
      ) => HasSigningScheme s where


  data Signature s :: * -> Type

  type VKey s :: Type

  type SKey s :: Type

  type Signable s :: Type -> Constraint

  sign :: Signable s a => a -> SKey s -> Signature s a

  verify :: Signable s a => VKey s -> a -> Signature s a -> Bool
