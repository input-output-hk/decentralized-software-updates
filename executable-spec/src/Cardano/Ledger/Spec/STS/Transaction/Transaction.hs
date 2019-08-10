{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple STS that emulates transactions different from update transactions.
module Cardano.Ledger.Spec.STS.Transaction.Transaction where

import           Cardano.Prelude (HeapWords, heapWords)
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)

import           Ledger.Core (Slot, (⨃))

-- | Dummy transaction, containing some text.
newtype Transaction = Transaction { text :: Text }
  deriving (Eq, Ord, Show)

instance HeapWords Transaction where
  heapWords (Transaction text) = heapWords text

data TRANSACTION

data St = St { txToSlot :: Map Transaction Slot }
  deriving (Eq, Show)

instance STS TRANSACTION where

  type Environment TRANSACTION = Slot -- Current slot

  type State TRANSACTION = St

  type Signal TRANSACTION = Transaction

  data PredicateFailure TRANSACTION = NoFailure
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC (currentSlot, St { txToSlot }, tx) <- judgmentContext
      pure $! St { txToSlot = txToSlot ⨃ [(tx, currentSlot)] }
    ]

genTransaction :: Gen Transaction
genTransaction = Transaction <$> Gen.prune (Gen.text (Range.linear 0 25) Gen.ascii)

-- data TRANSACTIONS

-- instance STS TRANSACTIONS where

--   type Environment TRANSACTIONS = Slot

--   type State TRANSACTIONS = St

--   type Signal TRANSACTIONS = [Transaction]

--   data PredicateFailure TRANSACTIONS = NoFailures
--     deriving (Eq, Show)

--   initialRules = []

--   transitionRules = [
--     do
--       TRC (env, st, txs) <- judgmentContext
--       case txs of
--         [] -> pure $! st
--         (tx:txs') -> do
--           st' <- trans @TRANSACTION $ TRC(env, st, tx)
--           trans @TRANSACTIONS $ TRC (env, st', txs')
--     ]

-- instance Embed TRANSACTION TRANSACTIONS where
--   wrapFailed = error "TRANSACTION shouldn't fail"
