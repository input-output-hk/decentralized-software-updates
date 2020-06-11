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

module Cardano.Ledger.Update.Env.StakeDistribution
  ( StakeDistribution
  , Stake (Stake)
  , stakeMap
  , emptyStakeDistribution
  , fromList
  , totalStake
  , stakeOfKeys
  , stakeOfKeys'
  , addStake
  , stakeThreshold
  )
where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Sum (Sum), getSum)
import           Data.Set (Set)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           System.Random (Random)

import           Cardano.Ledger.Assert (assert, (==!))


newtype Stake = Stake { getStake :: Word64 }
 deriving stock (Generic)
 deriving newtype (Eq, Ord, Show, Enum, Num, Integral, Real, Random)
 deriving anyclass (NoUnexpectedThunks)

data StakeDistribution k =
  StakeDistribution
  { stakeMap    :: !(Map k Stake)
  , totalStake  :: !Stake
    -- ^ Total stake in the stake distribution.
    --
    -- INVARIANT:
    --
    -- > totalStake = Map.foldr' (+) (Stake 0) stakeMap
    --
  } deriving (Show, Generic, NoUnexpectedThunks)

checkInvariants :: StakeDistribution p -> StakeDistribution p
checkInvariants sd =
  assert (totalStake sd ==! Map.foldr' (+) (Stake 0) (stakeMap sd))
         sd

emptyStakeDistribution :: Ord k => StakeDistribution k
emptyStakeDistribution = checkInvariants $
  StakeDistribution
  { stakeMap   = mempty
  , totalStake = 0
  }

-- | Return the sum of the stake associated with each key.
stakeOfKeys
  :: Ord k
  => Map k b
  -> StakeDistribution k
  -> Stake
stakeOfKeys keyMap StakeDistribution { stakeMap }
  = Map.foldl' (+) 0 $ stakeMap `Map.intersection` keyMap

-- | Return the sum of the stake associated with each key.
--
-- Unlike 'stakeOfkeys', this function takes a set of keys instead of a 'Map'.
-- Using a 'Set' instead of a 'Map' is slower, so when possible, 'stakeOfkeys'
-- should be used.
--
stakeOfKeys'
  :: Ord k
  => Set k
  -> StakeDistribution k
  -> Stake
stakeOfKeys' keyset StakeDistribution { stakeMap } =
  Map.foldl' (+) 0 $ stakeMap `Map.restrictKeys` keyset

-- | Add the given stake amount to the stake of the given key.
--
-- If the key is not present in the map it is added to it, with the given stake.
addStake
  :: Ord k
  => k
  -> Stake
  -> StakeDistribution k
  -> StakeDistribution k
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
  :: Ord k
  => [(k, Stake)]
  -> StakeDistribution k
fromList xs
  = checkInvariants
  $ StakeDistribution
    { stakeMap   = Map.fromList xs
    , totalStake = foldl' (+) 0 $ fmap snd xs
    }

--------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------

-- | Given a total stake and the adversary stake ratio, compute the how much
-- stake of this total stake is needed to meet the voting threshold.
--
-- This definition is based on 'vThreshold'. We have that the total stake needed
-- for a given @r_a@ equals:
--
-- > vThreshold r_a / 100 * totalStake
-- > = { def. vThreshold }
-- > 50 * (r_a + 1) / 100 * totalStake
-- > = { algebra }
-- > 1/2 * (r_a + 1) * totalStake
--
stakeThreshold
  :: (RealFrac a)
  => a
  -> Stake
  -> Stake
stakeThreshold r_a totalStake =
  round $ 1/2 * (r_a + 1) * fromIntegral totalStake
