-- |

module Cardano.Ledger.Generators.QuickCheck
  ( kGen
  , currentSlotGen
  , participantsGen
  )
where

import           Control.Arrow ((&&&))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

import qualified Ledger.Core as Core

kGen :: Gen Core.BlockCount
kGen = Core.BlockCount <$> Gen.choose (1, 10)

currentSlotGen :: Gen Core.Slot
currentSlotGen = Core.Slot <$> Gen.choose (0, 100)

participantsGen :: Gen (Bimap Core.VKey Core.SKey)
participantsGen
  = pure
  $! Bimap.fromList
  $  fmap (Core.vKey &&& Core.sKey)
  $  fmap Core.keyPair
  $  fmap Core.Owner $ [0 .. 10]
