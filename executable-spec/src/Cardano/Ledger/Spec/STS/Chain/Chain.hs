{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Simple block-chain mock, to bundle transactions into blocks, including slot
-- ticks.
module Cardano.Ledger.Spec.STS.Chain.Chain where

import           Control.Arrow ((&&&))
import qualified Data.Bimap as Bimap
import           GHC.Generics (Generic)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot (Slot))
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION,
                     TRANSACTIONS)
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO
import           Cardano.Ledger.Spec.STS.Sized (Size, Sized, costsList, size)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Data (Commit, SIPData)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation


data CHAIN hashAlgo


data Env hashAlgo
  = Env
    { initialSlot :: !Slot
    , maximumBlockSize :: !Size
    -- ^ Maximum block size. The interpretation of this value depends on the
    -- instance of 'Sized'.
    --
    -- TODO: use abstract size instead.
    , transactionsEnv :: Environment (TRANSACTIONS hashAlgo)
    }
  deriving (Eq, Show)


data St hashAlgo
  = St
    { currentSlot :: !Slot
    , transactionsSt :: State (TRANSACTIONS hashAlgo)
    }
  deriving (Eq, Show)


data Block hashAlgo
  = Block
    { slot :: !Slot
    , transactions :: ![Signal (TRANSACTION hashAlgo)]
    }
    deriving (Eq, Show, Generic)

deriving instance ( HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Commit hashAlgo)
                  ) => HasTypeReps (Block hashAlgo)


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps (Commit hashAlgo)
         ) => Sized (Block hashAlgo) where
  costsList _ = costsList (undefined :: Signal (TRANSACTION hashAlgo))


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps (Commit hashAlgo)
         ) => STS (CHAIN hashAlgo) where

  type Environment (CHAIN hashAlgo) = Env hashAlgo

  type State (CHAIN hashAlgo) = (St hashAlgo)

  type Signal (CHAIN hashAlgo) = (Block hashAlgo)

  data PredicateFailure (CHAIN hashAlgo)
    = BlockSlotNotIncreasing CurrentSlot Slot
    | MaximumBlockSizeExceeded Size (Threshold Size)
    | TransactionsFailure (PredicateFailure (TRANSACTIONS hashAlgo))
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
      TRC ( Env { maximumBlockSize, transactionsEnv }
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
      let Transaction.Env
            { Transaction.updatesEnv = upE
            , Transaction.utxoEnv = utxoE
            } = transactionsEnv
      transactionsSt' <-
        trans @(TRANSACTIONS hashAlgo)
          $ TRC ( Transaction.Env slot upE utxoE
                , transactionsSt
                , transactions
                )
      pure $! St { currentSlot = slot
                 , transactionsSt = transactionsSt'
                 }
    ]


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps (Commit hashAlgo)
         ) => Embed (TRANSACTIONS hashAlgo) (CHAIN hashAlgo) where
  wrapFailed = TransactionsFailure


-- | Type wrapper that gives more information about what the 'Slot' represents.
newtype CurrentSlot = CurrentSlot Slot deriving (Eq, Show)


instance ( HasTypeReps hashAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps (Commit hashAlgo)
         , HasTypeReps (Hash hashAlgo SIPData)
         ) => HasTrace (CHAIN hashAlgo) where

  envGen _traceLength
    = Env <$> initialSlotGen
          <*> maxBlockSizeGen
          <*> (transactionsEnvGen initialSlotGen)
    where
      initialSlotGen = Slot <$> Gen.integral (Range.constant 0 100)
      -- For now we fix the maximum block size to an abstract size of 100
      maxBlockSizeGen = pure 100
      participantsGen = pure
                      $! Bimap.fromList
                      $  fmap (Core.vKey &&& Core.sKey)
                      $  fmap Core.keyPair
                      $  fmap Core.Owner $ [0 .. 10]
      transactionsEnvGen gSlot
        = Transaction.Env <$> gSlot
                          <*> updatesEnvGen gSlot
                          <*> (pure $ UTxO.Env)
      updatesEnvGen gs = Update.Env <$> gs <*> ideationEnvGen gs <*> implementationEnvGen gs
      ideationEnvGen gs = Ideation.Env <$> gs <*> participantsGen
      implementationEnvGen gs = Implementation.Env <$> gs

  sigGen Env { maximumBlockSize, transactionsEnv } St { currentSlot, transactionsSt } =
    Block <$> gNextSlot
          <*> gTransactions (Transaction.Env currentSlot updEnv utxoEnv)
                            transactionsSt
    where
      Transaction.Env { Transaction.updatesEnv = updEnv
                      , Transaction.utxoEnv = utxoEnv
                      } = transactionsEnv
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
      gTransactions = Transaction.transactionsGen maximumBlockSize
