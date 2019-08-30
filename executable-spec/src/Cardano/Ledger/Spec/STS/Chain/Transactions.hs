{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


-- TODO: For the sake of consistency with the other modules this should be
-- renamed to 'Transaction'
module Cardano.Ledger.Spec.STS.Chain.Transactions where

import           Cardano.Prelude (HeapWords, heapWords, heapWords1)
import           Data.Bimap (Bimap)
import           Data.Function ((&))
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)
import Data.Set (Set)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Control.State.Transition.Generator (sigGen, genTrace)
import           Control.State.Transition.Trace (traceSignals, TraceOrder(OldestFirst))

import           Ledger.Core (Slot)
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Sized (WordCount, size)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (TxIn, TxOut, Coin (Coin), Witness)
import qualified Cardano.Ledger.Spec.STS.Dummy.Transaction as Dummy
import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation
import Cardano.Ledger.Spec.STS.Update (UPDATES, ideationSt)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO



data TRANSACTIONS


data Env =
  Env { currentSlot :: Slot
      , participants :: Bimap Core.VKey Core.SKey
      , utxoEnv :: Environment UTXO
      }
  deriving (Eq, Show)


data St =
  St { utxoSt :: State UTXO
     , updateSt :: State UPDATES
     }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup St
  deriving Monoid via GenericMonoid St


-- | Transactions contained in a block.
data Tx
  = Tx
  { body :: TxBody
  , witnesses :: ![Witness]
  }
  deriving (Eq, Show)

data TxBody
  = TxBody
  { inputs :: !(Set TxIn)
  , outputs :: ![TxOut]
  , fees :: !Coin
  , update :: ![UpdatePayload]
    -- ^ Update payload
  } deriving (Eq, Show)

instance HeapWords Tx where
  -- TODO: consider using the abstract size instead.
  heapWords _ = 0
  -- (Dummy dummyTx) = heapWords1 dummyTx
  -- heapWords (Ideation ideationTx) = heapWords1 ideationTx


instance STS TRANSACTIONS where

  type Environment TRANSACTIONS = Env

  type State TRANSACTIONS = St

  type Signal TRANSACTIONS = [Tx]

  data PredicateFailure TRANSACTIONS = TxFailure (PredicateFailure TRANSACTION)
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
  wrapFailed = TxFailure


data TRANSACTION


instance STS TRANSACTION where

  type Environment TRANSACTION = Environment TRANSACTIONS

  type State TRANSACTION = State TRANSACTIONS

  type Signal TRANSACTION = Tx

  data PredicateFailure TRANSACTION = UpdatesFailure (PredicateFailure UPDATES)
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { currentSlot, utxoEnv, participants }
          , st@St { utxoSt, updateSt }
          , Tx { body = TxBody { inputs, outputs, fees, update}, witnesses }
          ) <- judgmentContext
      -- TODO: keep in mind that some of the update rules will have to be
      -- triggered in the header validation rules (which we currently don't
      -- have).
      --
      utxoSt' <- trans @UTXO $ TRC (utxoEnv, utxoSt, UTxO.Payload inputs outputs fees)
      -- UTXO and UPDATE transition systems should be independent, so it
      -- shouldn't matter which transition is triggered first. Even if the
      -- update mechanism can change fees, these changes should happen at epoch
      -- boundaries and at header rules.
      updateSt' <-
        trans @UPDATES $
          TRC ( Update.Env { Update.participants = participants
                           , Update.implementationEnv = Implementation.Env
                           }
              , updateSt
              , update
              )
      pure $ St { utxoSt = utxoSt'
                , updateSt = updateSt'
                }
    ]


instance Embed UTXO TRANSACTION where
  wrapFailed = error "UTXO transition shouldn't fail (yet)"


instance Embed UPDATES TRANSACTION where
  wrapFailed = UpdatesFailure


-- | Generate a list of 'Tx's that fit in the given maximum size.
transactionsGen
  :: WordCount
  -> Environment TRANSACTIONS
  -> State TRANSACTIONS
  -> Gen [Tx]
transactionsGen maximumSize env st
  =   fitTransactions . traceSignals OldestFirst
  -- TODO: check what is a realistic distribution for empty blocks, or disallow
  -- the generation of empty blocks altogether.
  <$> genTrace @TRANSACTION 30 env st transactionGen

  where
    -- Fit the transactions that fit in the given maximum block size.
    fitTransactions :: [Tx] -> [Tx]
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

    transactionGen (Env { participants }) (St { updateSt })
      -- TODO: figure out what a realistic distribution for update payload is.
      --
      -- TODO: do we need to model the __liveness__ assumption of the underlying
      -- protocol? That is, model the fact that honest party votes will be
      -- eventually comitted to the chain. Or is this implicit once we start
      -- generating votes uniformly distributed over all the parties (honest and
      -- otherwise)
      --
      -- We do not generate witnesses for now
      =   (`Tx` [])
      .   dummyBody
      <$> Gen.frequency
            [ (9, pure $! []) -- We don't generate payload in 9/10 of the cases.
            , (1, sigGen
                    @UPDATES
                    Update.Env { Update.participants = participants
                               , Update.implementationEnv = Implementation.Env
                               }
                    updateSt
              )
            ]
      where
        -- For now we don't generate inputs and outputs.
        dummyBody update
          = TxBody
            { inputs = mempty
            , outputs = mempty
            , fees = Coin
            , update = update
            }
