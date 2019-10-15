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
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Typeable (typeOf)
import           Data.Set (Set)
import           Data.Map.Strict (Map)
import           GHC.Generics (Generic)

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot, BlockCount)
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (TxIn, TxOut, Coin, Witness)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import           Cardano.Ledger.Spec.STS.Update (UPDATES)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Data (SIPData, Commit)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO


-- | Environment of the TRANSACTION STS
data Env hashAlgo =
  Env { k :: !BlockCount
      , currentSlot :: !Slot
      , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
      , participants :: Bimap Core.VKey Core.SKey
      , utxoEnv :: !(Environment UTXO)
      }
  deriving (Eq, Show, Generic)

-- | State of the TRANSACTION STS
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

deriving instance ( HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo SIPData)
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

deriving instance ( HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Commit hashAlgo)
                  ) => HasTypeReps (TxBody hashAlgo)


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Commit hashAlgo)
         , HasTypeReps (Hash hashAlgo SIPData)
         ) => Sized (Tx hashAlgo) where
  costsList _
    =  [ (typeOf (undefined :: TxIn), 1)
       , (typeOf (undefined :: TxOut), 1)
       , (typeOf (undefined :: Coin), 1)
       ]
    ++ costsList (undefined :: UpdatePayload hashAlgo)


data TRANSACTION hashAlgo

instance HashAlgorithm hashAlgo => STS (TRANSACTION hashAlgo) where

  type Environment (TRANSACTION hashAlgo) = Env hashAlgo

  type State (TRANSACTION hashAlgo) = St hashAlgo

  type Signal (TRANSACTION hashAlgo) = Tx hashAlgo

  data PredicateFailure (TRANSACTION hashAlgo)
    = TxFailure (PredicateFailure (UPDATES hashAlgo))
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { k
                , currentSlot
                , asips
                , participants
                , utxoEnv
                }
          , St { utxoSt
               , updateSt
               }
          , Tx { body = TxBody { inputs, outputs, fees, update} }
          ) <- judgmentContext

      utxoSt' <- trans @UTXO $ TRC (utxoEnv, utxoSt, UTxO.Payload inputs outputs fees)
      -- UTXO and UPDATE transition systems should be independent, so it
      -- shouldn't matter which transition is triggered first. Even if the
      -- update mechanism can change fees, these changes should happen at epoch
      -- boundaries and at header rules.

      updateSt' <-
        trans @(UPDATES hashAlgo) $
          TRC ( Update.Env { Update.k = k
                           , Update.currentSlot = currentSlot
                           , Update.asips = asips
                           , Update.participants =  participants
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
  wrapFailed = TxFailure
