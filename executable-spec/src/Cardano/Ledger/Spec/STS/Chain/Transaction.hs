{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Chain.Transaction where

import Control.Exception (assert)
import           Data.Bimap (Bimap)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Typeable (typeOf)
import           Data.Set (Set)
import           Data.Map.Strict (Map)
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot, BlockCount)
import qualified Ledger.Core as Core
import qualified Control.State.Transition.Trace.Generator.QuickCheck as Trace.QC

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (TxIn, TxOut, Coin (Coin), Witness)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import           Cardano.Ledger.Spec.STS.Update (UPDATES, UPDATE)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO


-- | Environment of the TRANSACTION STS
data Env hashAlgo =
  Env { k :: !BlockCount
      , currentSlot :: !Slot
      , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
      , participants :: Bimap Core.VKey Core.SKey
      , apprvsips :: !(Set (Data.SIPHash hashAlgo))
      , utxoEnv :: !(Environment UTXO)
      }
  deriving (Eq, Show, Generic)

-- | State of the TRANSACTION STS
data St hashAlgo =
  St { subsips :: !(Map (Data.Commit hashAlgo) (Data.SIP hashAlgo))
     , wssips :: !(Map (Data.Commit hashAlgo) Slot)
     , wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
     , sipdb :: !(Map (Data.SIPHash hashAlgo) (Data.SIP hashAlgo))
     , ballots :: !(Map (Data.SIPHash hashAlgo) (Map Core.VKey Data.Confidence))
     , implementationSt :: State (IMPLEMENTATION hashAlgo)
     , utxoSt :: State UTXO
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
                  , HasTypeReps (Hash hashAlgo Data.SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Data.Commit hashAlgo)
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
                  , HasTypeReps (Hash hashAlgo Data.SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Data.Commit hashAlgo)
                  ) => HasTypeReps (TxBody hashAlgo)


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Data.Commit hashAlgo)
         , HasTypeReps (Hash hashAlgo Data.SIPData)
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
                , apprvsips
                , utxoEnv
                }
          , St { subsips
               , wssips
               , wrsips
               , sipdb
               , ballots
               , implementationSt
               , utxoSt
               }
          , Tx { body = TxBody { inputs, outputs, fees, update} }
          ) <- judgmentContext

      utxoSt' <- trans @UTXO $ TRC (utxoEnv, utxoSt, UTxO.Payload inputs outputs fees)
      -- UTXO and UPDATE transition systems should be independent, so it
      -- shouldn't matter which transition is triggered first. Even if the
      -- update mechanism can change fees, these changes should happen at epoch
      -- boundaries and at header rules.

      Update.St { Update.subsips = subsips'
                , Update.wssips = wssips'
                , Update.wrsips = wrsips'
                , Update.sipdb = sipdb'
                , Update.ballots = ballots'
                , Update.implementationSt = implementationSt'
                } <-
        trans @(UPDATES hashAlgo) $
          TRC ( Update.Env { Update.k = k
                           , Update.currentSlot = currentSlot
                           , Update.asips = asips
                           , Update.participants =  participants
                           , Update.apprvsips = apprvsips
                           }
              , Update.St { Update.subsips = subsips
                          , Update.wssips = wssips
                          , Update.wrsips = wrsips
                          , Update.sipdb = sipdb
                          , Update.ballots = ballots
                          , Update.implementationSt = implementationSt
                          }
              , update
              )
      pure $ St { subsips = subsips'
                , wssips = wssips'
                , wrsips = wrsips'
                , sipdb = sipdb'
                , ballots = ballots'
                , implementationSt = implementationSt'
                , utxoSt = utxoSt'
                }
    ]


instance HashAlgorithm hashAlgo => Embed UTXO (TRANSACTION hashAlgo) where
  wrapFailed = error "UTXO transition shouldn't fail (yet)"


instance HashAlgorithm hashAlgo => Embed (UPDATES hashAlgo) (TRANSACTION hashAlgo) where
  wrapFailed = TxFailure

instance ( HasTypeReps hashAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps (Data.Commit hashAlgo)
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         ) => Trace.QC.HasTrace (TRANSACTION hashAlgo) () where

  -- Since we don't use the 'TRANSACTION' STS in isolation, we don't need a
  -- environment generator.
  envGen = undefined

  sigGen
    _traceGenEnv
    (Env { k
         , currentSlot
         , asips
         , participants
         }
    )
    (St { subsips
        , wssips
        , wrsips
        , ballots
        , voteResultSIPs
        , implementationSt
        }
    )
    -- TODO: figure out what a realistic distribution for update payload is.
    --
    -- TODO: do we need to model the __liveness__ assumption of the underlying
    -- protocol? That is, model the fact that honest party votes will be
    -- eventually comitted to the chain. Or is this implicit once we start
    -- generating votes uniformly distributed over all the parties (honest and
    -- otherwise)
    --
    = do
    someUpdatePayload <-
      QC.frequency
        [ (9, pure $! []) -- We don't generate payload in 9/10 of the cases.
        , (1, do
              someUpdatePayload <-
                Trace.QC.sigGen
                  @(UPDATES hashAlgo)
                  ()
                  Update.Env { Update.k = k
                             , Update.currentSlot = currentSlot
                             , Update.asips = asips
                             , Update.participants = participants
                             }
                  Update.St { Update.subsips = subsips
                            , Update.wssips = wssips
                            , Update.wrsips = wrsips
                            , Update.ballots = ballots
                            , Update.voteResultSIPs = voteResultSIPs
                            , Update.implementationSt = implementationSt
                            }
              pure $! someUpdatePayload
          )
        ]
    let
      someBody
        -- For now we don't generate inputs and outputs.
        = TxBody
            { inputs = mempty
            , outputs = mempty
            , fees = Coin
            , update = someUpdatePayload
            }
      -- We do not generate witnesses for now
      someWitnesses = []
    pure $! Tx { body = someBody, witnesses = someWitnesses }

  shrinkSignal Tx { body, witnesses } =
    assert (null witnesses) $ -- For now we rely on the set of witnesses being empty.
    fmap (mkTx . Trace.QC.shrinkSignal @(UPDATE hashAlgo) @()) (update body)
    where
      mkTx :: [UpdatePayload hashAlgo] -> Tx hashAlgo
      mkTx updatePayload = Tx { body = body', witnesses = [] }
        where
          body' = body { update = updatePayload }
