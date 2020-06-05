{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.StakeDistribution
  ( StakeDistribution
  , stakeMap
  , emptyStakeDistribution
  , fromList
  , totalStake
  , stakeOfKeys
  , stakeOfKeys'
  , addStake
  )
where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Sum (Sum), getSum)
import           Data.Set (Set)
import           GHC.Generics (Generic)

import           Cardano.Ledger.Assert (assert, (==!))

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

import           Cardano.Ledger.Spec.Classes.Hashable (Hash, Hashable)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     VKey)


data StakeDistribution p =
  StakeDistribution
  { stakeMap    :: !(Map (Hash p (VKey p)) Data.Stake)
  , totalStake  :: !Data.Stake
    -- ^ Total stake in the stake distribution.
    --
    -- INVARIANT:
    --
    -- > totalStake = Map.foldr' (+) (Data.Stake 0) stakeMap
    --
  } deriving (Generic)

deriving instance (Hashable p, HasSigningScheme p) => Show (StakeDistribution p)
instance NoUnexpectedThunks (Hash p (VKey p)) => NoUnexpectedThunks (StakeDistribution p)

checkInvariants :: StakeDistribution p -> StakeDistribution p
checkInvariants sd =
  assert (totalStake sd ==! Map.foldr' (+) (Data.Stake 0) (stakeMap sd))
         sd

emptyStakeDistribution :: Hashable p => StakeDistribution p
emptyStakeDistribution = checkInvariants $
  StakeDistribution
  { stakeMap   = mempty
  , totalStake = 0
  }

-- | Return the sum of the stake associated with each key.
stakeOfKeys
  :: Hashable p
  => Map (Hash p (VKey p)) b
  -> StakeDistribution p
  -> Data.Stake
stakeOfKeys keyMap StakeDistribution { stakeMap }
  = Map.foldl' (+) 0 $ stakeMap `Map.intersection` keyMap

-- | Return the sum of the stake associated with each key.
--
-- Unlike 'stakeOfkeys', this function takes a set of keys instead of a 'Map'.
-- Using a 'Set' instead of a 'Map' is slower, so when possible, 'stakeOfkeys'
-- should be used.
--
stakeOfKeys'
  :: Hashable p
  => Set (Hash p (VKey p))
  -> StakeDistribution p
  -> Data.Stake
stakeOfKeys' keyset StakeDistribution { stakeMap } =
  Map.foldl' (+) 0 $ stakeMap `Map.restrictKeys` keyset

-- | Add the given stake amount to the stake of the given key.
--
-- If the key is not present in the map it is added to it, with the given stake.
addStake
  :: Hashable p
  => Hash p (VKey p)
  -> Data.Stake
  -> StakeDistribution p
  -> StakeDistribution p
addStake hashKey stake StakeDistribution { stakeMap, totalStake }
  = checkInvariants
  $ StakeDistribution
    { stakeMap = updatedStakeMap
    , totalStake = totalStake + stake
    }
  where
    updatedStakeMap =
      Map.alter (fmap getSum . (Just (Sum stake) <>) . fmap Sum)
                hashKey
                stakeMap

fromList
  :: Hashable p
  => [(Hash p (VKey p), Data.Stake)]
  -> StakeDistribution p
fromList xs
  = checkInvariants
  $ StakeDistribution
    { stakeMap   = Map.fromList xs
    , totalStake = foldl' (+) 0 $ fmap snd xs
    }
