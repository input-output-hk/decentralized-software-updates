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
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot)

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (TxIn, TxOut, Coin, Witness)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import           Cardano.Ledger.Spec.STS.Update (UPDATES)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Data (SIPData, Commit)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO




--data TRANSACTIONS hashAlgo

-- | Environment of the TRANSACTION STS
data Env hashAlgo =
  Env { currentSlot :: !Slot
      , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
      -- ^ When a SIP will not be active any more
      -- (i.e., end of open for voting period)
      , updatesEnv :: !(Environment (UPDATES hashAlgo))
      -- ^ Environment of the child STS (UPDATES)
      , utxoEnv :: !(Environment UTXO)
      -- ^ Environment of the child STS (UTXO)
      }
  deriving (Eq, Show, Generic)

-- | State of the TRANSACTION STS
data St hashAlgo =
  St { wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
       -- ^ When a SIP was revealed
     , utxoSt :: State UTXO
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


-- instance HashAlgorithm hashAlgo => STS (TRANSACTIONS hashAlgo) where

--   type Environment (TRANSACTIONS hashAlgo) = Environment (TRANSACTION hashAlgo)

--   type State (TRANSACTIONS hashAlgo) = State (TRANSACTION hashAlgo)

--   type Signal (TRANSACTIONS hashAlgo) = [Tx hashAlgo]

--   data PredicateFailure (TRANSACTIONS hashAlgo)
--     = TxFailure (PredicateFailure (TRANSACTION hashAlgo))
--     deriving (Eq, Show)

--   initialRules = []

--   transitionRules = [
--     do
--       TRC (env, st, txs) <- judgmentContext
--       case txs of
--         [] -> pure $! st
--         (tx:txs') -> do
--           st' <- trans @(TRANSACTION hashAlgo) $ TRC (env, st, tx)
--           trans @(TRANSACTIONS hashAlgo) $ TRC (env, st', txs')
--     ]


-- instance HashAlgorithm hashAlgo => Embed (TRANSACTION hashAlgo) (TRANSACTIONS hashAlgo) where
--   wrapFailed = TxFailure


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
      TRC ( Env { currentSlot, asips, utxoEnv, updatesEnv }
          , St { wrsips
               , utxoSt
               , updateSt = Update.St { Update.ideationSt = idst
                                      , Update.implementationSt = impst
                                      }
               }
          , Tx { body = TxBody { inputs, outputs, fees, update} }
          ) <- judgmentContext

      utxoSt' <- trans @UTXO $ TRC (utxoEnv, utxoSt, UTxO.Payload inputs outputs fees)
      -- UTXO and UPDATE transition systems should be independent, so it
      -- shouldn't matter which transition is triggered first. Even if the
      -- update mechanism can change fees, these changes should happen at epoch
      -- boundaries and at header rules.
      let Update.Env { Update.currentSlot = _
                     , Update.asips = _
                     , Update.ideationEnv = idEnv
                     , Update.implementationEnv = implEnv
                     } = updatesEnv

      updateSt'@Update.St {Update.wrsips = wrsips'} <-
        trans @(UPDATES hashAlgo) $
          TRC ( Update.Env { Update.currentSlot = currentSlot
                           , Update.asips = asips
                           , Update.ideationEnv =  idEnv
                           , Update.implementationEnv = implEnv
                           }
              , Update.St { Update.wrsips = wrsips
                              -- pass the updated wrsip state
                          , Update.ideationSt = idst
                          , Update.implementationSt = impst
                          }
              , update
              )
      pure $ St { wrsips = wrsips'
                , utxoSt = utxoSt'
                , updateSt = updateSt'
                }
    ]


instance HashAlgorithm hashAlgo => Embed UTXO (TRANSACTION hashAlgo) where
  wrapFailed = error "UTXO transition shouldn't fail (yet)"


instance HashAlgorithm hashAlgo => Embed (UPDATES hashAlgo) (TRANSACTION hashAlgo) where
  wrapFailed = TxFailure

