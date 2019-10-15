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


module Cardano.Ledger.Spec.STS.Chain.Body where

import           Data.Function ((&))
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), IRC (IRC),initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Control.State.Transition.Generator (sigGen, genTrace)
import           Control.State.Transition.Trace (traceSignals, TraceOrder(OldestFirst))
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot)

import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION)
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import           Cardano.Ledger.Spec.STS.Sized (Size, size, Sized, costsList)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (Coin (Coin))
--import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import           Cardano.Ledger.Spec.STS.Update (UPDATES)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Data (SIPData, Commit)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation

-- The Block BODY STS
data BODY hashAlgo

data Env hashAlgo
  = Env
    { transactionEnv :: !(Environment (TRANSACTION hashAlgo))
    }
    deriving (Eq, Show)

data St hashAlgo
  = St
    { transactionSt :: !(State (TRANSACTION hashAlgo))
    }
    deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup (St hashAlgo)
    deriving Monoid via GenericMonoid (St hashAlgo)

data BBody hashAlgo
 = BBody
   { transactions :: ![Signal (TRANSACTION hashAlgo)]
   }
   deriving (Eq, Show, Generic)

deriving instance ( HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Commit hashAlgo)
                  ) => HasTypeReps (BBody hashAlgo)

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps (Commit hashAlgo)
         ) => Sized (BBody hashAlgo) where
  costsList _ = costsList (undefined :: Signal (TRANSACTION hashAlgo))


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps (Commit hashAlgo)
         ) => STS (BODY hashAlgo) where

  type Environment (BODY hashAlgo) = Env hashAlgo

  type State (BODY hashAlgo) = St hashAlgo

  type Signal (BODY hashAlgo) = BBody hashAlgo

  data PredicateFailure (BODY hashAlgo)
    =
     BodyFailure (PredicateFailure (TRANSACTION hashAlgo))
    deriving (Eq, Show)


  initialRules = [
    ]

  transitionRules = [
    do
      TRC ( env@Env { transactionEnv }
          , st@St { transactionSt }
          , BBody {transactions}
          ) <- judgmentContext
      case transactions of
        [] -> pure $! st
        (tx:txs') -> do
          transactionSt' <-
            trans @(TRANSACTION hashAlgo)
              $ TRC ( transactionEnv
                    , transactionSt
                    , tx
                    )
          trans @(BODY hashAlgo) $ TRC ( env
                                       , St { transactionSt = transactionSt' }
                                       , BBody txs'
                                       )
    ]


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         ) => Embed (TRANSACTION hashAlgo) (BODY hashAlgo) where
  wrapFailed = BodyFailure

-- | Generate a list of 'Tx's that fit in the given maximum size.
--
-- TODO: this seems to belong to transactions ...
transactionsGen
  :: forall hashAlgo
   . ( HashAlgorithm hashAlgo
     , HasTypeReps hashAlgo
     , HasTypeReps (Commit hashAlgo)
     , HasTypeReps (Hash hashAlgo SIPData)
     )
  => Size
  -> Environment (BODY hashAlgo)
  -> State (BODY hashAlgo)
  -> Gen [Transaction.Tx hashAlgo]
transactionsGen maximumSize (Env{transactionEnv}) (St {transactionSt})
  =   fitTransactions . traceSignals OldestFirst
  -- TODO: check what is a realistic distribution for empty blocks, or disallow
  -- the generation of empty blocks altogether.
  <$> genTrace @(TRANSACTION hashAlgo) 30 transactionEnv transactionSt transactionGen

  where
    -- Fit the transactions that fit in the given maximum block size.
    fitTransactions :: [Transaction.Tx hashAlgo] -> [Transaction.Tx hashAlgo]
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

    transactionGen  (Transaction.Env { Transaction.k
                                     , Transaction.currentSlot
                                     , Transaction.asips
                                     , Transaction.participants
                                     , Transaction.utxoEnv
                                     }
                    )
                    (Transaction.St { Transaction.updateSt = updateSt })
      -- TODO: figure out what a realistic distribution for update payload is.
      --
      -- TODO: do we need to model the __liveness__ assumption of the underlying
      -- protocol? That is, model the fact that honest party votes will be
      -- eventually comitted to the chain. Or is this implicit once we start
      -- generating votes uniformly distributed over all the parties (honest and
      -- otherwise)
      --
      -- We do not generate witnesses for now
      =   (`Transaction.Tx` [])
      .   dummyBody
      <$> Gen.frequency
            [ (9, pure $! []) -- We don't generate payload in 9/10 of the cases.
            , (1, sigGen
                    @(UPDATES hashAlgo)
                    Update.Env { Update.k = k
                               , Update.currentSlot = currentSlot
                               , Update.asips = asips
                               , Update.participants = participants
                               }
                    updateSt
              )
            ]
      where
        -- For now we don't generate inputs and outputs.
        dummyBody update
          = Transaction.TxBody
            { Transaction.inputs = mempty
            , Transaction.outputs = mempty
            , Transaction.fees = Coin
            , Transaction.update = update
            }
