{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple block-chain mock, to bundle transactions into blocks, including slot
-- ticks.
module Cardano.Ledger.Spec.STS.Chain.Chain where

import           Control.Arrow ((&&&))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Prelude (HeapWords, heapWords, heapWords2)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)

import           Ledger.Core (Slot (Slot))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Chain.Transactions (TRANSACTION,
                     TRANSACTIONS)
import qualified Cardano.Ledger.Spec.STS.Chain.Transactions as Transactions
import           Cardano.Ledger.Spec.STS.Sized (WordCount, size)


data CHAIN


data Env
  = Env
    { initialSlot :: !Slot
    , maximumBlockSize :: !WordCount
    -- ^ Maximum block size. For now we measure this in number of 'Word's using
    -- 'heapWords' from 'Cardano.Prelude.HeapWords'.
    , participants :: Bimap Core.VKey Core.SKey
    }
  deriving (Eq, Show)


data St
  = St
    { currentSlot :: !Slot
    , transactionsSt :: State TRANSACTIONS
    }
  deriving (Eq, Show)


data Block
  = Block
    { slot :: !Slot
    , transactions :: ![Signal TRANSACTION]
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
    | TransactionsFailure (PredicateFailure TRANSACTIONS)
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
      TRC ( Env { maximumBlockSize, participants }
          , St { currentSlot, transactionsSt }
          , block@Block{ slot, transactions }) <- judgmentContext
      currentSlot < slot
        ?! BlockSlotNotIncreasing (CurrentSlot currentSlot) slot
      size block < maximumBlockSize
        ?! MaximumBlockSizeExceeded (size block) (Threshold maximumBlockSize)
      transactionsSt' <-
        trans @TRANSACTIONS $ TRC ( Transactions.Env currentSlot participants
                                  , transactionsSt
                                  , transactions
                                  )
      pure $! St { currentSlot = slot
                 , transactionsSt = transactionsSt'
                 }
    ]


instance Embed TRANSACTIONS CHAIN where
  wrapFailed = TransactionsFailure


-- | Type wrapper that gives more information about what the 'Slot' represents.
newtype CurrentSlot = CurrentSlot Slot deriving (Eq, Show)


instance HasTrace CHAIN where

  envGen _traceLength = Env <$> currentSlotGen <*> maxBlockSizeGen <*> participantsGen
    where
      currentSlotGen = Slot <$> Gen.integral (Range.constant 0 100)
      -- For now we fix the maximum block size to 32 words.
      maxBlockSizeGen = pure 32
      participantsGen = pure
                      $! Bimap.fromList
                      $  fmap (Core.vKey &&& Core.sKey)
                      $  fmap Core.keyPair
                      $  fmap Core.Owner $ [0 .. 10]

  sigGen Env { maximumBlockSize, participants } St { currentSlot, transactionsSt } =
    Block <$> gNextSlot
          <*> gTransactions (Transactions.Env currentSlot participants)
                            transactionsSt
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
