{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Chain.Transactions where

import           Cardano.Prelude (HeapWords, heapWords, heapWords1)
import           Data.Bimap (Bimap)
import           Data.Function ((&))
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Control.State.Transition.Generator (sigGen, genTrace)
import           Control.State.Transition.Trace (traceSignals, TraceOrder(OldestFirst))

import           Ledger.Core (Slot)
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Sized (WordCount, size)
import qualified Cardano.Ledger.Spec.STS.Dummy.Transaction as Dummy
import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation

data TRANSACTIONS

data Env =
  Env { currentSlot :: Slot
      , participants :: Bimap Core.VKey Core.SKey
      }
  deriving (Eq, Show)

data St =
  St { dummySt :: State Dummy.TRANSACTION
     , ideationSt :: State IDEATION
     }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup St
  deriving Monoid via GenericMonoid St

data Transaction
  = Dummy (Signal Dummy.TRANSACTION)
  | Ideation (Signal Ideation.IDEATION)
  deriving (Eq, Show)

instance HeapWords Transaction where
  heapWords (Dummy dummyTx) = heapWords1 dummyTx
  heapWords (Ideation ideationTx) = heapWords1 ideationTx

instance STS TRANSACTIONS where

  type Environment TRANSACTIONS = Env

  type State TRANSACTIONS = St

  type Signal TRANSACTIONS = [Transaction]

  data PredicateFailure TRANSACTIONS = TransactionFailure (PredicateFailure TRANSACTION)
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC (env, st, txs) <- judgmentContext
      case txs of
        [] -> pure $! st
        (tx:txs') -> do
          st' <- trans @TRANSACTION $ TRC (env, st, tx)
          trans @TRANSACTIONS $ TRC (env, st', txs')
    ]

instance Embed TRANSACTION TRANSACTIONS where
  wrapFailed = TransactionFailure

data TRANSACTION

instance STS TRANSACTION where

  type Environment TRANSACTION = Environment TRANSACTIONS

  type State TRANSACTION = State TRANSACTIONS

  type Signal TRANSACTION = Transaction

  data PredicateFailure TRANSACTION = IdeationFailure (PredicateFailure IDEATION)
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { currentSlot, participants }
          , st@St { dummySt, ideationSt }
          , tx
          ) <- judgmentContext
      case tx of
        Dummy dtx -> do
          dummySt' <- trans @Dummy.TRANSACTION $ TRC(currentSlot, dummySt, dtx)
          pure st { dummySt = dummySt' }
        Ideation itx -> do
          ideationSt' <- trans @IDEATION $ TRC ( participants, ideationSt, itx )
          pure st { ideationSt = ideationSt' }
    ]


instance Embed Dummy.TRANSACTION TRANSACTION where
  wrapFailed = error "Dummy.TRANSACTION shouldn't fail"


instance Embed IDEATION TRANSACTION where
  wrapFailed = IdeationFailure


-- | Generate a list of 'Transaction's that fit in the given maximum size.
transactionsGen
  :: WordCount
  -> Environment TRANSACTIONS
  -> State TRANSACTIONS
  -> Gen [Transaction]
transactionsGen maximumSize env st
  =   fitTransactions . traceSignals OldestFirst
  <$> genTrace @TRANSACTION 30 env st transactionGen

  where
    -- Fit the transactions that fit in the given maximum block size.
    fitTransactions :: [Transaction] -> [Transaction]
    fitTransactions txs = zip txs (tail sizes)
                          -- We subtract to account for the block constructor
                          -- and the 'Word64' value of the slot.
                        & takeWhile ((< maximumSize - 5) . snd)
                        & fmap fst
      where
        -- We compute the cumulative sum of the transaction sizes. We add 3 to
        -- account for the list constructor.
        sizes :: [WordCount]
        sizes = scanl (\acc tx -> acc + size tx + 3) 0 txs

    transactionGen _ (Env { participants }) (St { ideationSt }) =
      Gen.frequency [ (9, Dummy <$> Dummy.genTransaction)
                    , (1, Ideation <$> sigGen @IDEATION Nothing participants ideationSt)
                    ]
