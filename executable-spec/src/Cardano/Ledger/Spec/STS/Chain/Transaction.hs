{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Chain.Transaction where

import           Control.Exception (assert)
import           Data.Bimap (Bimap)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Typeable (typeOf)
import           Data.Set (Set)
import           Data.Map.Strict (Map)
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC
import           Data.Typeable (Typeable)

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)
import           Cardano.Crypto.DSIGN.Class (SignedDSIGN)
import           Cardano.Crypto.DSIGN.Mock (mockSigned, MockDSIGN)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot, BlockCount)
import qualified Ledger.Core as Core
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (TxIn, TxOut, Coin (Coin), Witness)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload)
import           Cardano.Ledger.Spec.STS.Update (UPDATES)
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
data Tx hashAlgo dsignAlgo
  = Tx
  { body :: TxBody hashAlgo dsignAlgo
  , witnesses :: ![Witness]
  }
  deriving (Eq, Show, Generic)

deriving instance ( Typeable dsignAlgo
                  , HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo Data.SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Data.Commit hashAlgo)
                  , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo))
                  ) => HasTypeReps (Tx hashAlgo dsignAlgo)

data TxBody hashAlgo dsignAlgo
  = TxBody
  { inputs :: !(Set TxIn)
  , outputs :: ![TxOut]
  , fees :: !Coin
  , update :: ![UpdatePayload hashAlgo dsignAlgo]
    -- ^ Update payload
  } deriving (Eq, Show, Generic)

deriving instance ( Typeable dsignAlgo
                  , HasTypeReps hashAlgo
                  , HasTypeReps (Hash hashAlgo Data.SIPData)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Data.Commit hashAlgo)
                  , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo))
                  ) => HasTypeReps (TxBody hashAlgo dsignAlgo)


instance ( Typeable dsignAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Data.Commit hashAlgo)
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo))
         ) => Sized (Tx hashAlgo dsignAlgo) where
  costsList _
    =  [ (typeOf (undefined :: TxIn), 1)
       , (typeOf (undefined :: TxOut), 1)
       , (typeOf (undefined :: Coin), 1)
       ]
    ++ costsList (undefined :: UpdatePayload hashAlgo dsignAlgo)


data TRANSACTION hashAlgo dsignAlgo

instance HashAlgorithm hashAlgo => STS (TRANSACTION hashAlgo dsignAlgo) where

  type Environment (TRANSACTION hashAlgo dsignAlgo) = Env hashAlgo

  type State (TRANSACTION hashAlgo dsignAlgo) = St hashAlgo

  type Signal (TRANSACTION hashAlgo dsignAlgo) = Tx hashAlgo dsignAlgo

  data PredicateFailure (TRANSACTION hashAlgo dsignAlgo)
    = TxFailure (PredicateFailure (UPDATES hashAlgo dsignAlgo))
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
        trans @(UPDATES hashAlgo dsignAlgo) $
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


instance HashAlgorithm hashAlgo => Embed UTXO (TRANSACTION hashAlgo dsignAlgo) where
  wrapFailed = error "UTXO transition shouldn't fail (yet)"


instance HashAlgorithm hashAlgo => Embed (UPDATES hashAlgo dsignAlgo) (TRANSACTION hashAlgo dsignAlgo) where
  wrapFailed = TxFailure

instance ( HashAlgorithm hashAlgo
         ) => STS.Gen.HasTrace (TRANSACTION hashAlgo MockDSIGN) () where

  -- Since we don't use the 'TRANSACTION' STS in isolation, we don't need a
  -- environment generator.
  envGen = undefined

  sigGen
    _traceGenEnv
    (Env { k
         , currentSlot
         , asips
         , participants
         , apprvsips
         }
    )
    (St { subsips
        , wssips
        , wrsips
        , sipdb
        , ballots
        , implementationSt
        }
    )
    = do
    someUpdatePayload <-
      QC.frequency
        [ (9, pure $! []) -- We don't generate payload in 9/10 of the cases.
        , (1, do
              someUpdatePayload <-
                STS.Gen.sigGen
                  @(UPDATES hashAlgo MockDSIGN)
                  ()
                  Update.Env { Update.k = k
                             , Update.currentSlot = currentSlot
                             , Update.asips = asips
                             , Update.participants = participants
                             , Update.apprvsips = apprvsips
                             }
                  Update.St { Update.subsips = subsips
                            , Update.wssips = wssips
                            , Update.wrsips = wrsips
                            , Update.sipdb = sipdb
                            , Update.ballots = ballots
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
    mkTx <$> STS.Gen.shrinkSignal @(UPDATES hashAlgo MockDSIGN) @() (update body)
    where
      mkTx :: [UpdatePayload hashAlgo MockDSIGN] -> Tx hashAlgo MockDSIGN
      mkTx updatePayload = Tx { body = body', witnesses = [] }
        where
          body' = body { update = updatePayload }
