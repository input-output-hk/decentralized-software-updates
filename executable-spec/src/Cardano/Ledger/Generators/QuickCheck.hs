{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Generators.QuickCheck
  ( bounded
  , boundedWith
  , k
  , currentSlot
  , participants
  , prvNoQuorum
  , prvNoMajority
  , rA
  , stakeDist
  )
where

import           GHC.Exts (fromList)

import           Control.Arrow ((&&&))
import           Data.Word (Word8)
import           System.Random (Random)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

import           Cardano.Crypto.DSIGN.Mock (SignKeyDSIGN (SignKeyMockDSIGN),
                     VerKeyDSIGN (VerKeyMockDSIGN))

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.State.Participants
                     (Participants (Participants), vkeyHashes)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import qualified Cardano.Ledger.Spec.State.StakeDistribution as StakeDistribution
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

import           Cardano.Ledger.Test.Mock (Mock)

k :: Gen Core.BlockCount
-- Here we choose a small maximum value of k, since otherwise we need very long
-- traces to get stable signals. However, we want to test with very high values
-- of @k@.
k  = Core.BlockCount
   <$> Gen.elements [1,2]

currentSlot :: Gen Core.Slot
currentSlot = Core.Slot <$>  Gen.choose (0, 10)

participants :: Gen (Participants Mock)
participants
  = pure
  $! Participants
  $  fromList
  $  fmap (VerKeyMockDSIGN &&& SignKeyMockDSIGN)
  $  [0 .. 10]

-- | Given a 'Bounded' type, generate a value that is either near the lower
-- bound, or near the middle of the range, or near the upper bound. The
-- parameter determines the proximity of the generated value to either the lower
-- bound, middle value, or upper bound.
--
bounded :: (Random a, Bounded a, Integral a) => a -> Gen a
bounded = boundedWith minBound maxBound

-- | Generalized version of 'bounded'
boundedWith
  :: (Random a, Integral a)
  => a
  -- ^ Lower bound.
  -> a
  -- ^ Upper bound.
  -> a
  -- ^ How much the generated value can differ from the lower and upper bound,
  -- and middle of the interval.
  -> Gen a
boundedWith aMinBound aMaxBound maxDifference =
  Gen.oneof [ Gen.choose (aMinBound, aMinBound + maxDifference)
            , Gen.choose (mid - maxDifference, mid + maxDifference)
            , Gen.choose (aMaxBound - maxDifference, aMaxBound)
            ]
  where mid = aMinBound + (aMaxBound - aMinBound) `div` 2

-- | Protocol parameter: maximum number of revoting periods
-- due to a No Quorum result
prvNoQuorum :: Gen Word8
prvNoQuorum =  Gen.choose (0, 3)

-- | Protocol parameter: maximum number of revoting periods
-- due to a No Majority result
prvNoMajority :: Gen Word8
prvNoMajority = Gen.choose (0, 3)

rA :: Gen Float
rA = Gen.choose (0, 0.5)

stakeDist :: Participants Mock -> Gen (StakeDistribution Mock)
stakeDist someParticipants = do
  let hashes = vkeyHashes someParticipants
  stks <- Gen.oneof [ stakeDistUniform (length hashes)
                    , stakeDistSkewed (length hashes)
                    ]
  pure $ StakeDistribution.fromList $ zip hashes stks
  where
    stakeDistUniform :: Int -> Gen [Data.Stake]
    stakeDistUniform n = Gen.vectorOf n (Gen.choose (1, 20))

    -- | We define as "skewed" a distribution where the 20% of stakeholders owns
    -- more than 80% percent of the stake.
    stakeDistSkewed :: Int -> Gen [Data.Stake]
    stakeDistSkewed n = do
      stksBig <- Gen.vectorOf (round $ (fromIntegral n :: Double) * 0.20) (Gen.choose (1000, 1000 + 20))
      stksSmall <- Gen.vectorOf (n - length stksBig) (Gen.choose (1, 20))
      let stks = stksBig ++ stksSmall
      pure stks
