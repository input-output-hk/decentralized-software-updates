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
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core ( Slot (Slot)
                             , addSlot
                             )
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION,
                     TRANSACTIONS)
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO
import           Cardano.Ledger.Spec.STS.Sized (Size, Sized, costsList, size)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Data ( Commit
                                                     , SIPData
                                                     , SIPHash
                                                     , VotingPeriod(..)
                                                     , VPStatus(..)
                                                     , vpDurationToSlotCnt
                                                     )
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
    , openVotingPeriods :: !(Map (SIPHash hashAlgo) (VotingPeriod hashAlgo))
      -- ^ Records the open voting periods  per SIP
    , closedVotingPeriods :: !(Map (SIPHash hashAlgo) (VotingPeriod hashAlgo))
      -- ^ Records the closed voting periods per SIP
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
                 , openVotingPeriods = Map.empty
                 , closedVotingPeriods = Map.empty
                 , transactionsSt = mempty
                 }
    ]

  transitionRules = [
    do
      TRC ( Env { maximumBlockSize, transactionsEnv }
          , St  { currentSlot
                , openVotingPeriods
                , closedVotingPeriods
                , transactionsSt
                }
          , block@Block{ slot, transactions }
          ) <- judgmentContext
      currentSlot < slot
        ?! BlockSlotNotIncreasing (CurrentSlot currentSlot) slot
      size block < maximumBlockSize
        ?! MaximumBlockSizeExceeded (size block) (Threshold maximumBlockSize)
      -- TODO: we will need a header transition as well, where the votes are
      -- tallied.

      let Transaction.Env
            { Transaction.updatesEnv = upE
            , Transaction.utxoEnv = utxoE
            } = transactionsEnv

          (openVotingPeriods', closedVotingPeriods') = updateVotingPeriods openVotingPeriods closedVotingPeriods

          updateVotingPeriods open closed =
            let
              -- traverse all VPs of open Map and update their status
              updatedOpen =
                Map.map (
                          \vp@VotingPeriod {sipId, openingSlot, vpDuration} ->
                            if slot > addSlot openingSlot  (vpDurationToSlotCnt vpDuration)
                              then -- VP must close
                                VotingPeriod
                                   { sipId = sipId
                                   , openingSlot = openingSlot
                                   , closingSlot = slot
                                   , vpDuration = vpDuration
                                   , vpStatus = VPClosed
                                   }
                              else -- VP should remain open
                                vp
                        )
                        open

              -- extract all closed VPs from open Map
              newClosedVPs =
                Map.filter (\VotingPeriod {vpStatus} ->
                                vpStatus == VPClosed
                           )
                           updatedOpen
            in
              -- insert new closed VPs into closed Map and remove closed VPs from open Map
              ( Map.difference open newClosedVPs
              , Map.union newClosedVPs closed
              )

      -- NOTE: the TRANSACTIONS transition corresponds to the BODY transition in
      -- Byron and Shelley rules.
      transactionsSt' <-
        trans @(TRANSACTIONS hashAlgo)
          $ TRC ( Transaction.Env slot openVotingPeriods' closedVotingPeriods' upE utxoE
                , transactionsSt
                , transactions
                )
      pure $! St { currentSlot = slot
                 , openVotingPeriods = openVotingPeriods'
                 , closedVotingPeriods = closedVotingPeriods'
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
          <*> (transactionsEnvGen
                initialSlotGen
                openVotingPeriodsGen
                closedVotingPeriodsGen
              )
    where
      initialSlotGen = Slot <$> Gen.integral (Range.constant 0 100)
      -- TODO: Generate a realistic open voting periods Map
      openVotingPeriodsGen = pure $ Map.empty
      -- TODO: Generate a realistic closed voting periods Map
      closedVotingPeriodsGen = pure $ Map.empty
      -- For now we fix the maximum block size to an abstract size of 100
      maxBlockSizeGen = pure 100
      participantsGen = pure
                      $! Bimap.fromList
                      $  fmap (Core.vKey &&& Core.sKey)
                      $  fmap Core.keyPair
                      $  fmap Core.Owner $ [0 .. 10]
      transactionsEnvGen gSlot gOpenVotingPeriods gClosedVotingPeriods
        = Transaction.Env
          <$> gSlot
          <*> gOpenVotingPeriods
          <*> gClosedVotingPeriods
          <*> updatesEnvGen gSlot gOpenVotingPeriods gClosedVotingPeriods
          <*> (pure $ UTxO.Env)
      updatesEnvGen gs gOpenVotingPeriods gClosedVotingPeriods =
        Update.Env
          <$> gs
          <*> gOpenVotingPeriods
          <*> gClosedVotingPeriods
          <*> ideationEnvGen gs gOpenVotingPeriods gClosedVotingPeriods
          <*> implementationEnvGen gs
      ideationEnvGen gs gOpenVotingPeriods gClosedVotingPeriods =
        Ideation.Env
          <$> gs
          <*> participantsGen
          <*> gOpenVotingPeriods
          <*> gClosedVotingPeriods

      implementationEnvGen gs = Implementation.Env <$> gs

  sigGen  Env { maximumBlockSize, transactionsEnv }
          St  { currentSlot
              , openVotingPeriods
              , closedVotingPeriods
              , transactionsSt
              } =
    Block <$> gNextSlot
          <*> gTransactions ( Transaction.Env
                                currentSlot
                                openVotingPeriods
                                closedVotingPeriods
                                updEnv
                                utxoEnv
                            )
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
