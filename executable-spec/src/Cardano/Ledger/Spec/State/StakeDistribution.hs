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
  , StakeDistributionCompact  
  , emptyStakeDistribution
  , fromList
  , fromListCompact
  , totalStake
  , totalStakeCmp
  , stakeOfKeys
  , stakeOfKeysCompact
  , addStake
  )
where

import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid (Sum (Sum), getSum)
import           Data.Word (Word8)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hash, Hashable)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     VKey)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed, Key, Value,
                     lookup)
import           Cardano.Ledger.Spec.STS.Common.Compact (CompactHash)


data StakeDistributionCompact =
  StakeDistributionCompact
  { stakeMapCmp    :: !(Map CompactHash Data.Stake)
    -- ^ use compact hashes
  , totalStakeCmp  :: !Data.Stake
    -- ^ Total stake in the stake distribution.
    --
    -- INVARIANT:
    --
    -- > totalStake = Map.foldr' (+) (Data.Stake 0) stakeMap
    --
  } deriving (Show)

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
  }

deriving instance (Hashable p, HasSigningScheme p) => Show (StakeDistribution p)
-- deriving instance (Hashable p, HasSigningScheme p) => Core.Relation (StakeDistribution p)
-- deriving instance (Hashable p) => Indexed (StakeDistribution p)
-- deriving instance (Hashable p) => Semigroup (StakeDistribution p)
-- deriving instance (Hashable p) => Monoid (StakeDistribution p)

emptyStakeDistribution :: Hashable p => StakeDistribution p
emptyStakeDistribution =
  StakeDistribution
  { stakeMap = mempty
  , totalStake = 0
  }

instance Hashable p => Indexed (StakeDistribution p) where
  type Key (StakeDistribution p) = Hash p (VKey p)

  type Value (StakeDistribution p) = Data.Stake

  lookup key StakeDistribution { stakeMap } = Map.lookup key stakeMap

-- | Return percentage of the stake that the given keys have.
stakeOfKeys
  :: Hashable p
  => Map (Hash p (VKey p)) b
  -> StakeDistribution p
  -> Data.Stake
stakeOfKeys
  keyMap
  StakeDistribution
  { stakeMap
  }
  = Map.foldl (+) 0 $ stakeMap `Map.intersection` keyMap


stakeOfKeysCompact
  :: Map CompactHash b -- Map (Hash p (VKey p)) b
  -> StakeDistributionCompact
  -> Data.Stake
stakeOfKeysCompact
  keyMap
  StakeDistributionCompact
  { stakeMapCmp
  }
  = Map.foldl (+) 0 $ stakeMapCmp `Map.intersection` keyMap


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
  = StakeDistribution
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
  = StakeDistribution
    { stakeMap   = Map.fromList xs
    , totalStake = foldl' (+) 0 $ fmap snd xs
    }


fromListCompact
  :: [(CompactHash, Data.Stake)]
  -> StakeDistributionCompact
fromListCompact xs
  = StakeDistributionCompact
    { stakeMapCmp   = Map.fromList xs
    , totalStakeCmp = foldl' (+) 0 $ fmap snd xs
    }
