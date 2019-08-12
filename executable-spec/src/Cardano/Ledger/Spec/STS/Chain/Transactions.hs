{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Chain.Transactions where

import           Cardano.Prelude (HeapWords, heapWords)
import           Data.Function ((&))
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)

import           Ledger.Core (Slot)

import           Cardano.Ledger.Spec.STS.Sized (WordCount, size)
import qualified Cardano.Ledger.Spec.STS.Dummy.Transaction as Dummy

data TRANSACTIONS

data St =
  St { dummySt :: Dummy.St
--     , ideation :: Ideation.St
     }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup St
  deriving Monoid via GenericMonoid St

data Transaction
  = Dummy Dummy.Transaction
  deriving (Eq, Show)

instance HeapWords Transaction where
  heapWords (Dummy dummyTx) = heapWords dummyTx

instance STS TRANSACTIONS where

  type Environment TRANSACTIONS = Slot

  type State TRANSACTIONS = St

  type Signal TRANSACTIONS = [Transaction]

  data PredicateFailure TRANSACTIONS = NoFailures
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
  wrapFailed = error "TRANSACTION shouldn't fail"

data TRANSACTION

instance STS TRANSACTION where

  type Environment TRANSACTION = Environment TRANSACTIONS

  type State TRANSACTION = State TRANSACTIONS

  type Signal TRANSACTION = Transaction

  data PredicateFailure TRANSACTION = NoFailure
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC (env, st@St { dummySt }, tx) <- judgmentContext
      case tx of
        Dummy dtx -> do
          dummySt' <- trans @Dummy.TRANSACTION $ TRC(env, dummySt, dtx)
          pure st { dummySt = dummySt' }
    ]

instance Embed Dummy.TRANSACTION TRANSACTION where
  wrapFailed = error "Dummy.TRANSACTION shouldn't fail"

-- | Generate a list of 'Transaction's that fit in the given maximum size.
transactionsGen :: WordCount -> Gen [Transaction]
transactionsGen maximumSize =
  fitTransactions <$> Gen.list (Range.constant 0 100) Dummy.genTransaction

  where
    -- Fit the transactions that fit in the given maximum block size.
    fitTransactions :: [Dummy.Transaction] -> [Transaction]
    fitTransactions txs = zip txs (tail sizes)
                          -- We subtract to account for the block constructor
                          -- and the 'Word64' value of the slot.
                        & takeWhile ((< maximumSize - 5) . snd)
                        & fmap fst
                        & fmap Dummy
      where
        -- We compute the cumulative sum of the transaction sizes. We add 3 to
        -- account for the list constructor.
        sizes :: [WordCount]
        sizes = scanl (\acc tx -> acc + size tx + 3) 0 txs
