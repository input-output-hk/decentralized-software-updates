{-# LANGUAGE FlexibleContexts #-}

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

import           Control.Arrow ((&&&))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Word (Word8)
import           System.Random (Random)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

import           Cardano.Crypto.DSIGN.Class (VerKeyDSIGN)
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN,
                     SignKeyDSIGN (SignKeyMockDSIGN),
                     VerKeyDSIGN (VerKeyMockDSIGN))

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Update.Data (Stake (Stake))


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

participants :: Gen (Bimap (VerKeyDSIGN MockDSIGN) (SignKeyDSIGN MockDSIGN))
participants
  = pure
  $! Bimap.fromList
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

stakeDist :: Gen (Map (VerKeyDSIGN MockDSIGN) Stake)
-- TODO: the keys of this map should also be replaced by the hashes of the VerKeyDSIGN key.
stakeDist = do
  p <- participants
  let vkeys = Bimap.keys p
  stks <- Gen.vectorOf (length vkeys) (Gen.choose (1, 20))
  pure $ Map.fromList $ zip vkeys (Stake <$> stks)
