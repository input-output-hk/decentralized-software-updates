{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simple STS that emulates transactions different from update transactions.
module Cardano.Ledger.Spec.STS.Transaction.Transaction where

import           Cardano.Prelude (HeapWords, heapWords)
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Control.State.Transition (Environment, PredicateFailure, STS,
                     Signal, State, TRC (TRC), initialRules, judgmentContext,
                     transitionRules)

import           Ledger.Core (Slot, (⨃))

data TRANSACTION

-- | Dummy transaction, containing some text.
newtype Transaction = Transaction { text :: Text }
  deriving (Eq, Ord, Show)

instance HeapWords Transaction where
  heapWords (Transaction text) = heapWords text

data St = St { submitted :: Map Transaction Slot }
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
      TRC (currentSlot, St { submitted }, tx) <- judgmentContext
      pure $ St { submitted = submitted ⨃ [(tx, currentSlot)] }
    ]

genTransaction :: Gen Transaction
genTransaction = Transaction <$> Gen.prune (Gen.text (Range.linear 0 25) Gen.ascii)
