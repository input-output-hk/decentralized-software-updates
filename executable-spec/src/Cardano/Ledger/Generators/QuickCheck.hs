
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
import qualified Data.Map as Map
import           Data.Word (Word8)
import           System.Random (Random)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

import           Cardano.Crypto.DSIGN.Mock (SignKeyDSIGN (SignKeyMockDSIGN),
                     VerKeyDSIGN (VerKeyMockDSIGN))

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.State.Participants
                     (Participants (Participants), vkeyHashes)
import           Cardano.Ledger.Spec.State.StakeDistribution
                     (StakeDistribution (StakeDistribution))
import           Cardano.Ledger.Spec.STS.Update.Data (Stake (Stake))

import           Cardano.Ledger.Test.Mock (Mock)

k :: Gen Core.BlockCount
-- Here we choose a small maximum value of k, since otherwise we need very long
-- traces to get stable signals. However, we want to test with very high values
-- of @k@.
k  = Core.BlockCount
  <$> Gen.frequency [ (99, Gen.choose (1, 10))
                    , (1, pure maxBound)
                    ]

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

prvNoQuorum :: Gen Word8
prvNoQuorum = Gen.choose (3, 7)

prvNoMajority :: Gen Word8
prvNoMajority = Gen.choose (3, 7)

rA :: Gen Float
rA = Gen.choose (0, 0.5)

stakeDist :: Participants Mock -> Gen (StakeDistribution Mock)
stakeDist someParticipants = do
  let hashes = vkeyHashes someParticipants
  stks <- Gen.vectorOf (length hashes) (Gen.choose (1, 20))
  pure $ StakeDistribution $ Map.fromList $ zip hashes (Stake <$> stks)
