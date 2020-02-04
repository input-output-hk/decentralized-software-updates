{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.WhenActive where

import           Data.Map.Strict (Map)

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)


newtype WhenActive a = WhenActive (Map a Core.Slot)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)
