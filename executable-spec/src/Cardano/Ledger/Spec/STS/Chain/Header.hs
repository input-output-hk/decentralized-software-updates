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


module Cardano.Ledger.Spec.STS.Chain.Header where

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

import           Cardano.Ledger.Spec.STS.Sized (Size, size, Sized, costsList)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (TxIn, TxOut, Coin (Coin), Witness)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import           Cardano.Ledger.Spec.STS.Update (UPDATES)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Data (SIPData, Commit, SIPHash, VotingPeriod)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO



-- | The Block HEADER STS
data HEADER

data Env
  = Env { initialSlot :: !Slot
        , hupdateEnv :: !Environment (HUPDATE hashAlgo)
        }
        deriving (Eq, Show)

data St hashAlgo
 = St { currentSlot :: !Slot
      , hupdateSt :: !State (HUPDATE hashAlgo)
      }
      deriving (Eq, Show)

data BHeader
  = BHeader
   { slot :: !Slot
   }
   deriving (Eq, Show, Generic)

deriving instance ( HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Commit hashAlgo)
                  ) => HasTypeReps (BHeader hashAlgo)

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps (Commit hashAlgo)
         ) => Sized (BHeader hashAlgo) where
  costsList _ = costsList (undefined :: Slot)

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps (Commit hashAlgo)
         ) => STS (HEADER hashAlgo) where

  type Environment (HEADER hashAlgo) = Env hashAlgo

  type State (HEADER hashAlgo) = St hashAlgo

  type Signal (HEADER hashAlgo) = BHeader

  data PredicateFailure (HEADER hashAlgo)
    = BlockSlotNotIncreasing CurrentSlot Slot
    | TransactionsFailure (PredicateFailure (HUPDATE hashAlgo))
    deriving (Eq, Show)


  initialRules = [
    do
      IRC Env { initialSlot } <- judgmentContext
      pure $! St { currentSlot = initialSlot }
    ]

  transitionRules = [
    do
      TRC ( Env { initialSlot, hupdateEnv@HUpdate.Env {currentSlot} }
          , St  { currentSlot
                , hupdateSt
                }
          , bheader@BHeader { slot }
          ) <- judgmentContext

      currentSlot < slot
        ?! BlockSlotNotIncreasing (CurrentSlot currentSlot) slot
  hupdateSt' <-
    trans @(HUPDATE hashAlgo)
      $ TRC ( hupdateEnv { currentSlot = slot }
            , hupdateSt
            , slot
            )
  pure $ St { currentSlot = slot
            , hupdateSt = hupdateSt'
            }


instance HashAlgorithm hashAlgo => Embed (HUPDATE hashAlgo) (HEADER hashAlgo) where
  wrapFailed = HeaderFailure
