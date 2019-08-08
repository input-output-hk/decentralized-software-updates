{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple block-chain mock, to bundle transactions into blocks, including slot
-- ticks.
module Cardano.Ledger.Spec.STS.Chain.Chain where

import           Data.Function ((&))
import           Data.Map.Strict (Map)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Prelude (HeapWords, heapWords, heapWords2)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Ledger.Core (Slot (Slot))

import           Cardano.Ledger.Spec.STS.Sized (WordCount, size)
import           Cardano.Ledger.Spec.STS.Transaction.Transaction (TRANSACTIONS)
import qualified Cardano.Ledger.Spec.STS.Transaction.Transaction as Dummy


data CHAIN

data St
  = St
    { currentSlot :: !Slot
    , submitted :: Map Dummy.Transaction Slot
    }
  deriving (Eq, Show)


data Env
  = Env
    { initialSlot :: !Slot
    , maximumBlockSize :: !WordCount
    -- ^ Maximum block size. For now we measure this in number of 'Word's using
    -- 'heapWords' from 'Cardano.Prelude.HeapWords'.
    }
  deriving (Eq, Show)

data Block
  = Block
    { slot :: !Slot
    , transactions :: ![Transaction]
    }
    deriving (Eq, Show)

instance HeapWords Block where
  heapWords (Block (Slot s) transactions) = heapWords2 s transactions

data Transaction
  = Dummy Dummy.Transaction
  deriving (Eq, Show)

unDummy :: Transaction -> Dummy.Transaction
unDummy (Dummy t) = t

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
      pure $! St { currentSlot = initialSlot
                 , submitted = mempty
                 }
    ]


  transitionRules = [
    do
      TRC ( Env { maximumBlockSize }
          , St { currentSlot, submitted }
          , block@Block{ slot, transactions }) <- judgmentContext
      currentSlot < slot
        ?! BlockSlotNotIncreasing (CurrentSlot currentSlot) slot
      size block < maximumBlockSize
        ?! MaximumBlockSizeExceeded (size block) (Threshold maximumBlockSize)
      Dummy.St submitted' <- trans @TRANSACTIONS $ TRC ( slot
                                                       , Dummy.St submitted
                                                       , unDummy <$> transactions
                                                       )
      pure $! St { currentSlot = slot
                 , submitted = submitted'
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
      gTransactions =
        fitTransactions <$> Gen.list (Range.constant 0 100) Dummy.genTransaction

        where
          -- Fit the transactions that fit in the given maximum block size.
          fitTransactions :: [Dummy.Transaction] -> [Transaction]
          fitTransactions txs = zip txs (tail sizes)
                              -- We subtract to account for the block constructor
                              -- and the 'Word64' value of the slot.
                              & takeWhile ((< maximumBlockSize - 5) . snd)
                              & fmap fst
                              & fmap Dummy
            where
              -- We compute the cumulative sum of the transaction sizes. We add 3 to
              -- account for the list constructor.
              sizes :: [WordCount]
              sizes = scanl (\acc tx -> acc + size tx + 3) 0 txs
