{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Spec.Classes.HasSigningScheme where

import Data.Kind (Type)
import GHC.Exts (Constraint)


class ( forall a . Eq (Signature s a)
      , forall a . Show (Signature s a)
      , Eq (VKey s) -- TODO: check whether it is ok to demand this from verifying keys.
      , Show (VKey s)
      , Show (SKey s)
      ) => HasSigningScheme s where

  data Signature s :: * -> Type

  type VKey s :: Type

  type SKey s :: Type

  type Signable s :: Type -> Constraint

  sign :: Signable s a => a -> SKey s -> Signature s a

  verify :: Signable s a => VKey s -> a -> Signature s a -> Bool

class ( HasSigningScheme s
      , Signable s (SignedPayload a)
      ) => Signed s a | a -> s where

  type SignedPayload a

  signedPayload :: a -> SignedPayload a

  signedBy :: a -> VKey s

  payloadSignature :: a -> Signature s (SignedPayload a)

  signatureVerifies :: a -> Bool
  signatureVerifies a =
    verify (signedBy a) (signedPayload a) (payloadSignature a)
