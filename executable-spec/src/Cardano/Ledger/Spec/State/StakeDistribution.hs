{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.StakeDistribution where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Word (Word8)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Data (Stake)
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hash, Hashable)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     VKey)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed)


newtype StakeDistribution p = StakeDistribution (Map (Hash p (VKey p)) Data.Stake)

deriving instance (Hashable p, HasSigningScheme p) => Show (StakeDistribution p)
deriving instance (Hashable p, HasSigningScheme p) => Core.Relation (StakeDistribution p)
deriving instance (Hashable p) => Indexed (StakeDistribution p)
deriving instance (Hashable p) => Semigroup (StakeDistribution p)
deriving instance (Hashable p) => Monoid (StakeDistribution p)


-- | Returns the total stake from a stake distribution.
totalStake :: StakeDistribution p -> Data.Stake
totalStake (StakeDistribution stakeMap) =
  -- TODO: we need to check for overflows!
  Map.foldr' (+) (Data.Stake 0) stakeMap

-- | Returns a map showing the percent @[0,100]@ of stake ownership of each
-- stakeholder.
-- stakeDistPct
--   :: StakeDistribution p
--   -> Map (Hash p (VKey p)) Word8
-- stakeDistPct stakeDistribution@(StakeDistribution stakeMap) =
--   Map.map (\st -> Data.stakePercentRound st $ totalStake stakeDistribution) stakeMap

stakePercentOfKeys
  :: Ord (Hash p (VKey p))
  => [Hash p (VKey p)]
  -> (StakeDistribution p, Stake)
  -> Word8
stakePercentOfKeys keys stakeDist@(StakeDistribution stakeMap, totStake)
  = round @Double
  $ (fromIntegral (totalStake (StakeDistribution keysStakemap))
     /
    fromIntegral totStake -- (totalStake stakeDist)
    )
    *
    100
  where
    keysStakemap = -- Map.restrictKeys stakeMap (Set.fromList keys)
      Map.dropWhileAntitone (\k -> k `notElem` keys ) stakeMap

-- -- | Return percentage of the stake that the given keys have.
-- -- Single pass over the stakeMap
-- stakePercentOfKeys
--   :: Ord (Hash p (VKey p))
--   => [Hash p (VKey p)]
--   -> StakeDistribution p
--   -> Word8
-- stakePercentOfKeys keys stakeDist@(StakeDistribution stakeMap)
--   = 
--   let 
--     keysmap = Map.fromList $ zip keys (repeat "")
--     (totStake, filteredStake) = singlePass keysmap 
--                                            stakeDist
--   in round @Double
--     $ (fromIntegral (filteredStake))
--        /
--       (fromIntegral (totStake))
--       *
--       100
--   where
--     -- computes with a single pass the total stake and the 
--     -- stake of a specific set of keys
--     singlePass 
--       :: Ord (Hash p (VKey p)) 
--       => Map.Map (Hash p (VKey p)) String
--       -> StakeDistribution p 
--       -> (Data.Stake, Data.Stake)
--     singlePass keysm 
--                stakeDist@(StakeDistribution stakeMap) = 
--       Map.foldrWithKey' (\hsh stk accum -> doit keysm
--                                                 hsh 
--                                                 stk 
--                                                 accum
--                         )
--                         (Data.Stake 0, Data.Stake 0)
--                         stakeMap
--         where
--           doit kmap h s acc@(totSt, filteredSt) = 
--             case Map.lookup h kmap of
--               Nothing -> (totSt + s, filteredSt)
--               Just _ -> (totSt + s, filteredSt + s)
