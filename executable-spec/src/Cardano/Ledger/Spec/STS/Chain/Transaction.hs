{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Cardano.Ledger.Spec.STS.Chain.Transaction where

import           Data.Bimap (Bimap)
import           Data.Function ((&))
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)
import           Data.Typeable (typeOf)
import Data.Set (Set)

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Control.State.Transition.Generator (sigGen, genTrace)
import           Control.State.Transition.Trace (traceSignals, TraceOrder(OldestFirst))
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot)
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Sized (Size, size, Sized, costsList)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (TxIn, TxOut, Coin (Coin), Witness)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation
import           Cardano.Ledger.Spec.STS.Update (UPDATES)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Data (SIPData, Commit)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO




data TRANSACTIONS hashAlgo


data Env =
  Env { currentSlot :: Slot
      , participants :: Bimap Core.VKey Core.SKey
      , utxoEnv :: Environment UTXO
      }
  deriving (Eq, Show, Generic)


data St hashAlgo =
  St { utxoSt :: State UTXO
     , updateSt :: State (UPDATES hashAlgo)
     }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup (St hashAlgo)
  deriving Monoid via GenericMonoid (St hashAlgo)


-- | Transactions contained in a block.
data Tx hashAlgo
  = Tx
  { body :: TxBody hashAlgo
  , witnesses :: ![Witness]
  }
  deriving (Eq, Show, Generic)

deriving instance ( HasTypeReps (Hash hashAlgo SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Commit hashAlgo)
                  ) => HasTypeReps (Tx hashAlgo)

data TxBody hashAlgo
  = TxBody
  { inputs :: !(Set TxIn)
  , outputs :: ![TxOut]
  , fees :: !Coin
  , update :: ![UpdatePayload hashAlgo]
    -- ^ Update payload
  } deriving (Eq, Show, Generic)

deriving instance ( HasTypeReps (Hash hashAlgo SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Commit hashAlgo)
                  ) => HasTypeReps (TxBody hashAlgo)


instance ( HashAlgorithm hashAlgo
         , HasTypeReps (Commit hashAlgo)
         , HasTypeReps (Hash hashAlgo SIPData)
         ) => Sized (Tx hashAlgo) where
  costsList _
    =  [ (typeOf (undefined :: TxIn), 1)
       , (typeOf (undefined :: TxOut), 1)
       , (typeOf (undefined :: Coin), 1)
       ]
    ++ costsList (undefined :: UpdatePayload hashAlgo)


instance HashAlgorithm hashAlgo => STS (TRANSACTIONS hashAlgo) where

  type Environment (TRANSACTIONS hashAlgo) = Env

  type State (TRANSACTIONS hashAlgo) = St hashAlgo

  type Signal (TRANSACTIONS hashAlgo) = [Tx hashAlgo]

  data PredicateFailure (TRANSACTIONS hashAlgo) = TxFailure (PredicateFailure (TRANSACTION hashAlgo))
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC (env, st, txs) <- judgmentContext
      case txs of
        [] -> pure $! st
        (tx:txs') -> do
          st' <- trans @(TRANSACTION hashAlgo) $ TRC (env, st, tx)
          trans @(TRANSACTIONS hashAlgo) $ TRC (env, st', txs')
    ]


instance HashAlgorithm hashAlgo => Embed (TRANSACTION hashAlgo) (TRANSACTIONS hashAlgo) where
  wrapFailed = TxFailure


data TRANSACTION hashAlgo


instance HashAlgorithm hashAlgo => STS (TRANSACTION hashAlgo) where

  type Environment (TRANSACTION hashAlgo) = Environment (TRANSACTIONS hashAlgo)

  type State (TRANSACTION hashAlgo) = State (TRANSACTIONS hashAlgo)

  type Signal (TRANSACTION hashAlgo) = Tx hashAlgo

  data PredicateFailure (TRANSACTION hashAlgo)
    = UpdatesFailure (PredicateFailure (UPDATES hashAlgo))
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { utxoEnv, participants }
          , St { utxoSt, updateSt }
          , Tx { body = TxBody { inputs, outputs, fees, update} }
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
        trans @(UPDATES hashAlgo) $
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


instance HashAlgorithm hashAlgo => Embed UTXO (TRANSACTION hashAlgo) where
  wrapFailed = error "UTXO transition shouldn't fail (yet)"


instance HashAlgorithm hashAlgo => Embed (UPDATES hashAlgo) (TRANSACTION hashAlgo) where
  wrapFailed = UpdatesFailure


-- | Generate a list of 'Tx's that fit in the given maximum size.
transactionsGen
  :: forall hashAlgo
   . ( HashAlgorithm hashAlgo
     , HasTypeReps (Commit hashAlgo)
     , HasTypeReps (Hash hashAlgo SIPData)
     )
  => Size
  -> Environment (TRANSACTIONS hashAlgo)
  -> State (TRANSACTIONS hashAlgo)
  -> Gen [Tx hashAlgo]
transactionsGen maximumSize env st
  =   fitTransactions . traceSignals OldestFirst
  -- TODO: check what is a realistic distribution for empty blocks, or disallow
  -- the generation of empty blocks altogether.
  <$> genTrace @(TRANSACTION hashAlgo) 30 env st transactionGen

  where
    -- Fit the transactions that fit in the given maximum block size.
    fitTransactions :: [Tx hashAlgo] -> [Tx hashAlgo]
    fitTransactions txs = zip txs (tail sizes)
                          -- We subtract to account for the block constructor
                          -- and the 'Word64' value of the slot.
                        & takeWhile ((< maximumSize - 5) . snd)
                        & fmap fst
      where
        -- We compute the cumulative sum of the transaction sizes. We add 3 to
        -- account for the list constructor.
        sizes :: [Size]
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
                    @(UPDATES hashAlgo)
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
