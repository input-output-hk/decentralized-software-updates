-- | Common generators used in the different 'HasTrace' instances.

module Cardano.Ledger.Generators
  ( kGen
  , currentSlotGen
  , participantsGen
  , r_aGen
  , stakeDistGen
  , prvNoQuorumGen
  , prvNoMajorityGen
  )
where

import           Control.Arrow ((&&&))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word8)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Ledger.Core as Core
import           Cardano.Ledger.Spec.STS.Update.Data (Stake, Stake(Stake))


prvNoQuorumGen :: Gen Word8
prvNoQuorumGen = Gen.integral (Range.constant 3 7)

prvNoMajorityGen :: Gen Word8
prvNoMajorityGen = Gen.integral (Range.constant 3 7)

stakeDistGen :: Gen (Map Core.VKey Stake)
stakeDistGen = do
  p <- participantsGen
  let vkeys = Bimap.keys p
  stks <- Gen.list (Range.singleton $ length vkeys) (Gen.word64 (Range.linear 1 100))
  pure $ Map.fromList $ zip vkeys (Stake <$> stks)

-- voteTGen :: Gen VThreshold
-- voteTGen = Gen.integral (Range.constant 50 75)

-- | Generates an adversary ratio in [0,0.5)
r_aGen :: Gen Float
r_aGen = Gen.float (Range.constant 0 0.5)

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
