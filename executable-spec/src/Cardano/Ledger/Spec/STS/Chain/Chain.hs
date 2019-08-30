{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           GHC.Generics (Generic)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot (Slot))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Chain.Transactions (TRANSACTION,
                     TRANSACTIONS)
import qualified Cardano.Ledger.Spec.STS.Chain.Transactions as Transactions
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO
import           Cardano.Ledger.Spec.STS.Sized (Size, Sized, costsList, size)


data CHAIN


data Env
  = Env
    { initialSlot :: !Slot
    , maximumBlockSize :: !Size
    -- ^ Maximum block size. The interpretation of this value depends on the
    -- instance of 'Sized'.
    --
    -- TODO: use abstract size instead.
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
    deriving (Eq, Show, Generic, HasTypeReps)


instance Sized Block where
  costsList _ = costsList (undefined :: Signal TRANSACTION)


instance STS CHAIN where

  type Environment CHAIN = Env

  type State CHAIN = St

  type Signal CHAIN = Block

  data PredicateFailure CHAIN
    = BlockSlotNotIncreasing CurrentSlot Slot
    | MaximumBlockSizeExceeded Size (Threshold Size)
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
          , block@Block{ slot, transactions }
          ) <- judgmentContext
      currentSlot < slot
        ?! BlockSlotNotIncreasing (CurrentSlot currentSlot) slot
      size block < maximumBlockSize
        ?! MaximumBlockSizeExceeded (size block) (Threshold maximumBlockSize)
      -- TODO: we will need a header transition as well, where the votes are
      -- tallied.

      -- NOTE: the TRANSACTIONS transition corresponds to the BODY transition in
      -- Byron and Shelley rules.
      transactionsSt' <-
        trans @TRANSACTIONS $ TRC ( Transactions.Env currentSlot participants UTxO.Env
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
      -- For now we fix the maximum block size to an abstract size of 100
      maxBlockSizeGen = pure 100
      participantsGen = pure
                      $! Bimap.fromList
                      $  fmap (Core.vKey &&& Core.sKey)
                      $  fmap Core.keyPair
                      $  fmap Core.Owner $ [0 .. 10]

  sigGen Env { maximumBlockSize, participants } St { currentSlot, transactionsSt } =
    Block <$> gNextSlot
          <*> gTransactions (Transactions.Env currentSlot participants UTxO.Env)
                            transactionsSt
    where
      -- We'd expect the slot increment to be 1 with high probability.
      --
      -- TODO: check the exact probability of having an empty slot.
      --
      gNextSlot =  Slot . (s +) <$> Gen.frequency [ (99, pure 1)
                                                  , (1, pure 2)
                                                  ]
        where
          Slot s = currentSlot

      -- We generate a list of transactions that fit in the maximum block size.
      gTransactions = Transactions.transactionsGen maximumBlockSize
