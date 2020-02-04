{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Ledger.Spec.Classes.HasAuthor where

import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey)

class HasAuthor a p | a -> p where
  author :: a -> VKey p
