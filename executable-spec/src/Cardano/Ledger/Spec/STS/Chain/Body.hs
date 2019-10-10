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
import           Data.Typeable (typeOf)
import Data.Set (Set)
import           Data.Map.Strict (Map)


import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Control.State.Transition.Generator (sigGen, genTrace)
import           Control.State.Transition.Trace (traceSignals, TraceOrder(OldestFirst))
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot)

import           Cardano.Ledger.Spec.STS.Chain.Transaction (TRANSACTION)
import qualified Cardano.Ledger.Spec.STS.Chain.Transaction as Transaction
import           Cardano.Ledger.Spec.STS.Sized (Size, size, Sized, costsList)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (TxIn, TxOut, Coin (Coin), Witness)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import           Cardano.Ledger.Spec.STS.Update (UPDATES)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Data (SIPData, Commit, SIPHash, VotingPeriod)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO


-- The Block BODY STS
data BODY hashAlgo

data Env hashAlgo
  = Env
    { currentSlot :: !Slot
    , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
    -- ^ When a SIP will not be active any more
    -- (i.e., end of open for voting period)
    , transactionEnv :: Environment (TRANSACTION hashAlgo)
    }
    deriving (Eq, Show)

data St hashAlgo
  = St
    { wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
      -- ^ When a SIP was revealed
    , transactionSt :: State (TRANSACTION hashAlgo)
    }
    deriving (Eq, Show)


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
     TransactionFailure (PredicateFailure (TRANSACTION hashAlgo))
    deriving (Eq, Show)


  initialRules = []

  transitionRules = [
    do
      TRC ( env@Env {currentSlot, asips}
          , St {wrsips}
          , bbody@BBody {transactions}
          ) <- judgmentContext
      case transactions of
        [] -> pure $! st
        (tx:txs') -> do
          st'@Transaction.St { wrsips = wrsips'
                             } <- trans @(TRANSACTION hashAlgo) $ TRC ( Transaction.Env
                                                        { currentSlot
                                                        , asips
                                                        }
                                                     , Transaction.St {wrsips}
                                                     , tx
                                                     )
          trans @(BODY hashAlgo) $ TRC ( env
                                       , St { wrsips = wrsips'
                                            , transactionsSt = st'
                                            }
                                       , txs'
                                       )
    ]


instance HashAlgorithm hashAlgo => Embed (TRANSACTION hashAlgo) (BODY hashAlgo) where
  wrapFailed = TxFailure

