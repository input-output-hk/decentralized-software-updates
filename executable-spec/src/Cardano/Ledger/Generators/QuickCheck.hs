
module Cardano.Ledger.Generators.QuickCheck
  ( bounded
  , boundedWith
  , kGen
  , currentSlotGen
  , participantsGen
  )
where

import           Control.Arrow ((&&&))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           System.Random (Random)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

import qualified Ledger.Core as Core

kGen :: Gen Core.BlockCount
-- Here we choose a small maximum value of k, since otherwise we need very long
-- traces to get stable signals. However, we want to test with very high values
-- of @k@.
kGen
  = Core.BlockCount
  <$> Gen.frequency [ (99, Gen.choose (1, 10))
                    , (1, pure maxBound)
                    ]

currentSlotGen :: Gen Core.Slot
currentSlotGen = Core.Slot <$>  Gen.choose (0, 10)

participantsGen :: Gen (Bimap Core.VKey Core.SKey)
participantsGen
  = pure
  $! Bimap.fromList
  $  fmap (Core.vKey &&& Core.sKey)
  $  fmap Core.keyPair
  $  fmap Core.Owner $ [0 .. 10]

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
