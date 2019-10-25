-- | Common generators used in the different 'HasTrace' instances.

module Cardano.Ledger.Generators
  ( kGen
  , currentSlotGen
  , participantsGen
  , voteTGen
  )
where

import           Control.Arrow ((&&&))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Ledger.Core as Core
import           Cardano.Ledger.Spec.STS.Update.Data (VThreshold)


voteTGen :: Gen VThreshold
voteTGen = Gen.integral (Range.constant 50 75)

kGen :: Gen Core.BlockCount
kGen = Core.BlockCount <$> Gen.integral (Range.linear 1 10)

currentSlotGen :: Gen Core.Slot
currentSlotGen = Core.Slot <$> Gen.integral (Range.constant 0 100)

participantsGen :: Gen (Bimap Core.VKey Core.SKey)
participantsGen
  = pure
  $! Bimap.fromList
  $  fmap (Core.vKey &&& Core.sKey)
  $  fmap Core.keyPair
  $  fmap Core.Owner $ [0 .. 10]
