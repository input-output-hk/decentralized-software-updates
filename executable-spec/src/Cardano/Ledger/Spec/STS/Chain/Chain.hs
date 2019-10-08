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

import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION)
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
import qualified Cardano.Ledger.Spec.STS.Chain.Body as Body
import qualified Cardano.Ledger.Spec.STS.Chain.Header as Header


data CHAIN hashAlgo


data Env hashAlgo
  = Env
    {
      maximumBlockSize :: !Size
    -- ^ Maximum block size. The interpretation of this value depends on the
    -- instance of 'Sized'.
    --
    -- TODO: use abstract size instead.
    , headerEnv :: Environment (HEADER hashAlgo)
    , bodyEnv :: Environment (BODY hashAlgo)
    }
    deriving (Eq, Show)


data St hashAlgo
  = St
    { headerSt :: State (HEADER hashAlgo)
    , bodySt :: State (BODY hashAlgo)
    }
    deriving (Eq, Show)


data Block hashAlgo
  = Block
    { header :: !Signal (HEADER hashAlgo)
    , body :: !Signal (BODY hashAlgo)
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

  type State (CHAIN hashAlgo) = St hashAlgo

  type Signal (CHAIN hashAlgo) = Block hashAlgo

  data PredicateFailure (CHAIN hashAlgo)
    = MaximumBlockSizeExceeded Size (Threshold Size)
    | TransactionsFailure (PredicateFailure (BODY hashAlgo))
    | TransactionsFailure (PredicateFailure (HEADER hashAlgo))
    deriving (Eq, Show)


  initialRules = [
    do
       IRC Env { maximumBlockSize } <- judgmentContext
       pure $! St { maximumBlockSize = 100
                      -- For now we fix the maximum block size to an abstract size of 100
                  , headerSt = mempty
                  , bodySt = mempty
                  }
    ]

  transitionRules = [
    do
      TRC ( Env { maximumBlockSize, headerEnv, bodyEnv@Body.Env{transactionEnv} }
          , St  { headerSt
                , bodySt@Body.St{transactionsSt}
                }
          , block@Block{ header, body }
          ) <- judgmentContext
      size block < maximumBlockSize
        ?! MaximumBlockSizeExceeded (size block) (Threshold maximumBlockSize)

      -- First a HEAD transition in order to update the state
      headerSt'@Header.St { currentSlot = slot'
                          , hupdateSt = HUpdate.St { wrsips = wrsips'
                                                   , asips = asips'
                                                   }
                          }  <-
        trans @(HEADER hashAlgo)
          $ TRC ( headerEnv
                , headerSt
                , header
                )

      -- Second a BODY transition with the updated state from header
      bodySt'<- trans @(BODY hashAlgo)
                  $ TRC ( bodyEnv@Body.Env
                           { currentSlot = slot'
                           , asips = asips'
                           , transactionEnv
                           }
                        , bodySt@Body.St
                            { wrsips = wrsips'
                            , transactionsSt
                            }
                        , body
                        )
      pure $! St { headerSt'
                 , bodySt'
                 }

      -- let Transaction.Env
      --       { Transaction.updatesEnv = upE
      --       , Transaction.utxoEnv = utxoE
      --       } = transactionsEnv

      --     (openVotingPeriods', closedVotingPeriods') = updateVotingPeriods openVotingPeriods closedVotingPeriods

      --     updateVotingPeriods open closed =
      --       let
      --         -- traverse all VPs of open Map and update their status
      --         updatedOpen =
      --           Map.map (
      --                     \vp@VotingPeriod {sipId, openingSlot, vpDuration} ->
      --                       if slot > addSlot openingSlot  (vpDurationToSlotCnt vpDuration)
      --                         then -- VP must close
      --                           VotingPeriod
      --                              { sipId = sipId
      --                              , openingSlot = openingSlot
      --                              , closingSlot = slot
      --                              , vpDuration = vpDuration
      --                              , vpStatus = VPClosed
      --                              }
      --                         else -- VP should remain open
      --                           vp
      --                   )
      --                   open

      --         -- extract all closed VPs from open Map
      --         newClosedVPs =
      --           Map.filter (\VotingPeriod {vpStatus} ->
      --                           vpStatus == VPClosed
      --                      )
      --                      updatedOpen
      --       in
      --         -- insert new closed VPs into closed Map and remove closed VPs from open Map
      --         ( Map.difference open newClosedVPs
      --         , Map.union newClosedVPs closed
      --         )

      -- -- NOTE: the TRANSACTIONS transition corresponds to the BODY transition in
      -- -- Byron and Shelley rules.
      -- transactionsSt'@Transaction.St { Transaction.openVotingPeriods = ovp'
      --                                   -- ovp' = returned state updated by IDEATION
      --                                , Transaction.utxoSt = _
      --                                , Transaction.updateSt = _
      --                                } <-
      --   trans @(TRANSACTIONS hashAlgo)
      --     $ TRC ( Transaction.Env slot closedVotingPeriods' upE utxoE
      --             -- pass the updated openVotingPeriods state
      --           , Transaction.St openVotingPeriods' utxoSt updateSt
      --           , transactions
      --           )
      -- pure $! St { currentSlot = slot
      --            , openVotingPeriods = ovp' -- This state has been further updated
      --                                       -- by the IDEATON STS
      --            , closedVotingPeriods = closedVotingPeriods'
      --            , transactionsSt = transactionsSt'
      --            }
    ]


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps (Commit hashAlgo)
         ) => Embed (BODY hashAlgo) (CHAIN hashAlgo) where
  wrapFailed = TransactionsFailure

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps (Commit hashAlgo)
         ) => Embed (HEADER hashAlgo) (CHAIN hashAlgo) where
  wrapFailed = TransactionsFailure


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
                closedVotingPeriodsGen
              )
    where
      initialSlotGen = Slot <$> Gen.integral (Range.constant 0 100)
      -- TODO: Generate a realistic closed voting periods Map
      closedVotingPeriodsGen = pure $ Map.empty
      -- For now we fix the maximum block size to an abstract size of 100
      maxBlockSizeGen = pure 100
      participantsGen = pure
                      $! Bimap.fromList
                      $  fmap (Core.vKey &&& Core.sKey)
                      $  fmap Core.keyPair
                      $  fmap Core.Owner $ [0 .. 10]
      transactionsEnvGen gSlot gClosedVotingPeriods
        = Transaction.Env
          <$> gSlot
          <*> gClosedVotingPeriods
          <*> updatesEnvGen gSlot gClosedVotingPeriods
          <*> (pure $ UTxO.Env)
      updatesEnvGen gs gClosedVotingPeriods =
        Update.Env
          <$> gs
          <*> gClosedVotingPeriods
          <*> ideationEnvGen gs gClosedVotingPeriods
          <*> implementationEnvGen gs
      ideationEnvGen gs gClosedVotingPeriods =
        Ideation.Env
          <$> gs
          <*> participantsGen
          <*> gClosedVotingPeriods

      implementationEnvGen gs = Implementation.Env <$> gs

  sigGen  Env { maximumBlockSize, transactionsEnv }
          St  { currentSlot
              --, openVotingPeriods
              , closedVotingPeriods
              , transactionsSt
              } =
    Block <$> gNextSlot
          <*> gTransactions ( Transaction.Env
                                currentSlot
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
