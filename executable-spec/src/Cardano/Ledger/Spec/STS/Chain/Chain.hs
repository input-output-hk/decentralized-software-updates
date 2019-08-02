{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple block-chain mock, to bundle transactions into blocks, including slot
-- ticks.
module Cardano.Ledger.Spec.STS.Chain.Chain where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Control.State.Transition (Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext, transitionRules, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Ledger.Core (Slot (Slot))


data CHAIN

data St
  = St
    { currentSlot :: !Slot
    }
  deriving (Eq, Show)


data Env
  = Env
    { initialSlot :: !Slot }
  deriving (Eq, Show)

data Block
  = Block
    { slot :: !Slot }

instance STS CHAIN where

  type Environment CHAIN = Env

  type State CHAIN = St

  type Signal CHAIN = Block

  data PredicateFailure CHAIN
    = BlockSlotNotIncreasing CurrentSlot Slot
    deriving (Eq, Show)


  initialRules = [
    do
      IRC Env { initialSlot } <- judgmentContext
      pure $! St { currentSlot = initialSlot }
    ]


  transitionRules = [
    do
      TRC (_, St { currentSlot }, Block { slot }) <- judgmentContext
      currentSlot < slot
        ?! BlockSlotNotIncreasing (CurrentSlot currentSlot) slot
      pure $! St { currentSlot = slot }
    ]

-- | Type wrapper that gives more information about what the 'Slot' represents.
newtype CurrentSlot = CurrentSlot Slot deriving (Eq, Show)

instance HasTrace CHAIN where

  envGen _traceLength = Env <$> gCurrentSlot
    where
      gCurrentSlot = Slot <$> Gen.integral (Range.constant 0 100)


  sigGen _ _ St { currentSlot } =
    Block <$> gNextSlot
    where
      -- We'd expect the slot increment to be close to 1, even for large Gen's
      -- size numbers.
      gNextSlot =  Slot . (s +) <$> Gen.integral (Range.exponential 1 10)
      Slot s = currentSlot
