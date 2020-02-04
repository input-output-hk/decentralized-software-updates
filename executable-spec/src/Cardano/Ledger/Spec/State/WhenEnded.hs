{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.WhenEnded where

import           Data.Map.Strict (Map)

import           Ledger.Core (Relation, Slot)

import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)

newtype WhenEnded a = WhenEnded (Map a Slot)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Relation, Semigroup, Monoid, Indexed)
