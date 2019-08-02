{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple block-chain mock, to bundle transactions into blocks, including slot
-- ticks.
module Cardano.Ledger.Spec.STS.Chain.Chain where

import           Data.Word (Word64)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Prelude (HeapWords, heapWords, heapWords2)

import           Control.State.Transition (Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     transitionRules, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Ledger.Core (Slot (Slot))

import qualified Cardano.Ledger.Spec.STS.Transaction.Transaction as Dummy


data CHAIN

data St
  = St
    { currentSlot :: !Slot
    }
  deriving (Eq, Show)


data Env
  = Env
    { initialSlot :: !Slot
    , maximumBlockSize :: WordCount
    -- ^ Maximum block size. For now we measure this in number of 'Word's using
    -- 'heapWords' from 'Cardano.Prelude.HeapWords'.
    }
  deriving (Eq, Show)

newtype WordCount = WordCount Word64
  deriving (Eq, Num, Ord, Show)

data Block
  = Block
    { slot :: !Slot
    , transactions :: [Transaction]
    }
    deriving (Eq, Show)

instance HeapWords Block where
  heapWords (Block (Slot s) transactions) = heapWords2 s transactions

data Transaction = Dummy Dummy.Transaction
  deriving (Eq, Show)

instance HeapWords Transaction where
  heapWords (Dummy dummyTx) = heapWords dummyTx

instance STS CHAIN where

  type Environment CHAIN = Env

  type State CHAIN = St

  type Signal CHAIN = Block

  data PredicateFailure CHAIN
    = BlockSlotNotIncreasing CurrentSlot Slot
    | MaximumBlockSizeExceeded WordCount (Threshold WordCount)
    deriving (Eq, Show)


  initialRules = [
    do
      IRC Env { initialSlot } <- judgmentContext
      pure $! St { currentSlot = initialSlot }
    ]


  transitionRules = [
    do
      TRC ( Env { maximumBlockSize }
          , St { currentSlot }
          , block@Block{ slot }) <- judgmentContext
      currentSlot < slot
        ?! BlockSlotNotIncreasing (CurrentSlot currentSlot) slot
      blockSize block < maximumBlockSize
        ?! MaximumBlockSizeExceeded (blockSize block) (Threshold maximumBlockSize)
      pure $! St { currentSlot = slot }
    ]

blockSize :: Block -> WordCount
blockSize = WordCount . fromIntegral . heapWords

-- | Type wrapper that gives more information about what the 'Slot' represents.
newtype CurrentSlot = CurrentSlot Slot deriving (Eq, Show)

instance HasTrace CHAIN where

  envGen _traceLength = Env <$> gCurrentSlot <*> gMaxBlockSize
    where
      gCurrentSlot = Slot <$> Gen.integral (Range.constant 0 100)
      -- For now we fix the maximum block size to 1 KiB.
      gMaxBlockSize = pure 1024


  sigGen _ _ St { currentSlot } =
    Block <$> gNextSlot <*> gTransactions
    where
      -- We'd expect the slot increment to be close to 1, even for large Gen's
      -- size numbers.
      gNextSlot =  Slot . (s +) <$> Gen.integral (Range.exponential 1 10)
        where
          Slot s = currentSlot

      -- TODO: make sure the generated transactions fit in the maximum block size.
      gTransactions =
        fmap Dummy <$> Gen.list (Range.constant 0 1000) Dummy.genTransaction
