{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments#-}

-- | Simple block-chain mock, to bundle transactions into blocks, including slot
-- ticks.
module Cardano.Ledger.Spec.STS.Chain.Chain where

import           Data.Function ((&))
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     Threshold (Threshold), initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen)
import           Ledger.Core (Slot (Slot))
import           Cardano.Ledger.Spec.STS.Sized (WordCount, size)
import           Data.Map.Strict (Map, insert, mapKeys, filterWithKey)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Ledger.Core (Slot, (⨃))
import           Data.Either (either, fromLeft, isLeft)

import           Cardano.Ledger.Spec.STS.Transaction.Transaction (TRANSACTION)
import qualified Cardano.Ledger.Spec.STS.Transaction.Transaction as Dummy
import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import           Cardano.Ledger.Spec.STS.Chain.Data (EnvChain(..), StChain(..)
                  , Block(..), Transaction(..), EnvTransactions(..), StTransactions (..)
                  , unTransaction) -- unDummy)
import qualified Cardano.Ledger.Spec.STS.Chain.Data as Data
import qualified Cardano.Ledger.Spec.STS.Update.Data as DataUpd
import qualified  Cardano.Ledger.Spec.STS.Update.Allphases as UpdateAll

data TxSt = TxSt { txsubmitted :: Map Transaction Slot }
   deriving (Eq, Show)

-- data TRANSACTION

-- instance STS TRANSACTION where

--   type Environment TRANSACTION = Slot -- Current slot

--   type State TRANSACTION = TxSt

--   type Signal TRANSACTION = Transaction

--   data PredicateFailure TRANSACTION = NoFailure
--     deriving (Eq, Show)

--   initialRules = []

--   transitionRules = [
--     do
--       TRC (currentSlot, TxSt { txsubmitted }, tx) <- judgmentContext
--       pure $! TxSt {txsubmitted = insert tx currentSlot txsubmitted} -- TxSt { txsubmitted = txsubmitted ⨃ [(tx, currentSlot)] }
--     ]

data TRANSACTIONS

instance STS TRANSACTIONS where

  type Environment TRANSACTIONS = EnvTransactions -- Slot

  type State TRANSACTIONS = StTransactions -- TxSt

  type Signal TRANSACTIONS = [Transaction]

  data PredicateFailure TRANSACTIONS = NoFailures
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( env@EnvTransactions{currSlot, envIdeation }
          , st@StTransactions{txToSlot, stIdeation}
          , txs
          )
        <- judgmentContext
      case txs of
        [] -> pure $! st
        (tx:txs') ->
          do
            case unTransaction tx of
              Left dummytx ->
                do
                  stDummy' <- trans @TRANSACTION $ TRC
                    ( currSlot
                    , (Dummy.St $ mapKeys (\(Dummy tr) -> tr) $ filterWithKey (\t _ -> isLeft (unTransaction t)) txToSlot)
                    , dummytx
                    )
                  let
                    (Dummy.St dummytxToslot) = stDummy'
                    newStateFromDummy = StTransactions
                      { txToSlot
                          = mapKeys (\dummytx -> Dummy dummytx) dummytxToslot
                      , stIdeation = stIdeation
                      }
                  trans @TRANSACTIONS $ TRC (env, newStateFromDummy, txs')
              Right (DataUpd.Ideation ideationTx) ->
                do
                  stIdeation' <- trans @IDEATION $ TRC
                    ( envIdeation
                    , stIdeation
                    , ideationTx
                    )
                  let
                    (DataUpd.StIdeation updtxToslot commitedSIPs submittedSIPs revealedSIPS) = stIdeation'
                    newStateFromIdeation = StTransactions
                      { txToSlot
                          = mapKeys (\updatetx -> Update updatetx) updtxToslot
                      , stIdeation = stIdeation'
                      }
                  trans @TRANSACTIONS $ TRC (env, newStateFromIdeation, txs')
    ]

instance Embed Dummy.TRANSACTION TRANSACTIONS where
  wrapFailed = error "TRANSACTION shouldn't fail"

instance Embed Ideation.IDEATION TRANSACTIONS where
  wrapFailed = error "UPDATE TRANSACTION shouldn't fail"

data CHAIN

instance STS CHAIN where

  type Environment CHAIN = Data.EnvChain -- Data.Env

  type State CHAIN = Data.StChain -- Data.St

  type Signal CHAIN = Data.Block

  data PredicateFailure CHAIN
    = BlockSlotNotIncreasing Data.CurrentSlot Slot
    | MaximumBlockSizeExceeded WordCount (Threshold WordCount)
    deriving (Eq, Show)


  initialRules = [
    do
      IRC EnvChain { initialSlot } <- judgmentContext
      pure $! StChain
                 {  currentSlot = initialSlot
                 ,  transactionsSt =
                      StTransactions
                        Map.empty $
                        DataUpd.StIdeation
                          Map.empty
                          Map.empty
                          Set.empty
                          Set.empty
                 }
    ]

  transitionRules = [
    do
      TRC ( EnvChain  { maximumBlockSize
                      , transactionsEnv =
                          EnvTransactions { currSlot = cslot
                                          , envIdeation = envid
                                          }
                      }
          , StChain { currentSlot,  transactionsSt =
                                      StTransactions
                                        { txToSlot = txtoslot
                                        , stIdeation = stideation
                                        }
                    }
          , block@Block{ slot, transactions }) <- judgmentContext
      currentSlot < slot
        ?! BlockSlotNotIncreasing (Data.CurrentSlot currentSlot) slot
      size block < maximumBlockSize
        ?! MaximumBlockSizeExceeded (size block) (Threshold maximumBlockSize)
      transactionsNewState <- trans @TRANSACTIONS $ TRC
                                         (  EnvTransactions  { currSlot = slot
                                                            , envIdeation = envid
                                                            }
                                         ,  StTransactions
                                              { txToSlot = txtoslot
                                              , stIdeation = stideation
                                              }
                                         , transactions
                                         )
      pure $! StChain
                { currentSlot = slot
                , transactionsSt = transactionsNewState
                }
    ]

instance Embed TRANSACTIONS CHAIN where
  wrapFailed = error "TRANSACTIONS shouldn't fail"

instance HasTrace CHAIN where

  envGen _traceLength =
    do
      currentSlot <- gCurrentSlot
      EnvChain <$> gCurrentSlot <*> gMaxBlockSize <*> (gTransactionsEnv currentSlot)
    where
      gCurrentSlot = Slot <$> Gen.integral (Range.constant 0 100)
      -- For now we fix the maximum block size to 32 words.
      gMaxBlockSize = pure 32
      gTransactionsEnv s
        = EnvTransactions
          <$> pure s
          <*> Ideation.generateIdeationEnv

  sigGen
    _
    EnvChain { maximumBlockSize, transactionsEnv }
    StChain { currentSlot, transactionsSt } =
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
          --fitTransactions <$> Gen.list (Range.constant 0 100) Dummy.genTransaction
          fitTransactions <$> (
            Gen.list (Range.constant 0 100) $
              Gen.frequency [ (1, Dummy <$> Dummy.genTransaction)
                            , (99, (Update . DataUpd.Ideation) <$>
                                    Ideation.genIdeationTx stakeholders submittedSIPs)
                            ]
            )
          where
            stakeholders = transactionsEnv & envIdeation & DataUpd.stakeholders
            submittedSIPs = transactionsSt & stIdeation & DataUpd.submittedSIPs

            -- Fit the transactions that fit in the given maximum block size.
            fitTransactions :: [Transaction] -> [Transaction]
            fitTransactions txs = zip txs (tail sizes)
                                -- We subtract to account for the block constructor
                                -- and the 'Word64' value of the slot.
                                & takeWhile ((< maximumBlockSize - 5) . snd)
                                & fmap fst

            -- fitTransactions :: [Dummy.Transaction] -> [Transaction]
            -- fitTransactions txs = zip txs (tail sizes)
            --                     -- We subtract to account for the block constructor
            --                     -- and the 'Word64' value of the slot.
            --                     & takeWhile ((< maximumBlockSize - 5) . snd)
            --                     & fmap fst
            --                     & fmap Dummy
              where
                -- We compute the cumulative sum of the transaction sizes. We add 3 to
                -- account for the list constructor.
                sizes :: [WordCount]
                sizes = scanl (\acc tx -> acc + size tx + 3) 0 txs
