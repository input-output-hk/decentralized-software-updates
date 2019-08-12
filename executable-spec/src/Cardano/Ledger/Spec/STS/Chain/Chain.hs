{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple block-chain mock, to bundle transactions into blocks, including slot
-- ticks.
module Cardano.Ledger.Spec.STS.Chain.Chain where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Prelude (HeapWords, heapWords, heapWords2)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Ledger.Core (Slot (Slot))

import           Cardano.Ledger.Spec.STS.Chain.Transactions (TRANSACTIONS)
import qualified Cardano.Ledger.Spec.STS.Chain.Transactions as Transactions
import           Cardano.Ledger.Spec.STS.Sized (WordCount, size)


data CHAIN

data Env
  = Env
    { initialSlot :: !Slot
    , maximumBlockSize :: !WordCount
    -- ^ Maximum block size. For now we measure this in number of 'Word's using
    -- 'heapWords' from 'Cardano.Prelude.HeapWords'.
    }
  deriving (Eq, Show)

data St
  = St
    { currentSlot :: !Slot
    , transactionsSt :: Transactions.St
    }
  deriving (Eq, Show)

data Block
  = Block
    { slot :: !Slot
    , transactions :: ![Transactions.Transaction]
    }
    deriving (Eq, Show)

instance HeapWords Block where
  heapWords (Block (Slot s) transactions) = heapWords2 s transactions


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
      pure $! St { currentSlot = initialSlot
                 , transactionsSt = mempty
                 }
    ]


  transitionRules = [
    do
      TRC ( Env { maximumBlockSize }
          , St { currentSlot, transactionsSt }
          , block@Block{ slot, transactions }) <- judgmentContext
      currentSlot < slot
        ?! BlockSlotNotIncreasing (CurrentSlot currentSlot) slot
      size block < maximumBlockSize
        ?! MaximumBlockSizeExceeded (size block) (Threshold maximumBlockSize)
      transactionsSt' <-
        trans @TRANSACTIONS $ TRC ( slot
                                  , transactionsSt
                                  , transactions
                                  )
      pure $! St { currentSlot = slot
                 , transactionsSt = transactionsSt'
                 }
    ]

instance Embed TRANSACTIONS CHAIN where
  wrapFailed = error "TRANSACTIONS shouldn't fail"

-- | Type wrapper that gives more information about what the 'Slot' represents.
newtype CurrentSlot = CurrentSlot Slot deriving (Eq, Show)

instance HasTrace CHAIN where

  envGen _traceLength = Env <$> gCurrentSlot <*> gMaxBlockSize
    where
      gCurrentSlot = Slot <$> Gen.integral (Range.constant 0 100)
      -- For now we fix the maximum block size to 32 words.
      gMaxBlockSize = pure 32


  sigGen _ Env { maximumBlockSize } St { currentSlot } =
    Block <$> gNextSlot <*> gTransactions
    where
      -- We'd expect the slot increment to be 1 with high probability.
      --
      -- TODO: check the exact probability of having an empty slot.
      gNextSlot =  Slot . (s +) <$> Gen.frequency [ (99, pure 1)
                                                  , (1, pure 2)
                                                  ]
        where
          Slot s = currentSlot

      -- We generate a list of transactions that fit in the maximum block size.
      gTransactions = Transactions.transactionsGen maximumBlockSize
