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

import           Control.Exception (assert)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Typeable (typeOf, Typeable)
import           Data.Set (Set)
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot, BlockCount)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme)
import           Cardano.Ledger.Spec.State.ActiveSIPs (ActiveSIPs)
import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs)
import           Cardano.Ledger.Spec.State.Ballot (Ballot)
import           Cardano.Ledger.Spec.State.WhenRevealedSIPs (WhenRevealedSIPs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSIPs (WhenSubmittedSIPs)
import           Cardano.Ledger.Spec.State.Participants (Participants)
import           Cardano.Ledger.Spec.State.RevealedSIPs (RevealedSIPs)
import           Cardano.Ledger.Spec.State.SubmittedSIPs (SubmittedSIPs)
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (TxIn, TxOut, Coin (Coin), Witness)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload, UPDATES)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO


-- | Environment of the TRANSACTION STS
data Env p =
  Env { k :: !BlockCount
      , currentSlot :: !Slot
      , asips :: !(ActiveSIPs p)
      , participants :: !(Participants p)
      , apprvsips :: !(ApprovedSIPs p)
      , utxoEnv :: !(Environment UTXO)
      }
  deriving (Show, Generic)

-- | State of the TRANSACTION STS
data St p =
  St { subsips :: !(SubmittedSIPs p)
     , wssips :: !(WhenSubmittedSIPs p)
     , wrsips :: !(WhenRevealedSIPs p)
     , sipdb :: !(RevealedSIPs p)
     , ballots :: !(Ballot p)
     , implementationSt :: State (IMPLEMENTATION p)
     , utxoSt :: State UTXO
     }
  deriving (Show, Generic)
  deriving Semigroup via GenericSemigroup (St p)
  deriving Monoid via GenericMonoid (St p)

-- | Transactions contained in a block.
data Tx p
  = Tx
  { body :: TxBody p
  , witnesses :: ![Witness]
  }
  deriving (Show, Generic)

deriving instance ( Typeable p
                  , HasTypeReps (TxBody p)
                  ) => HasTypeReps (Tx p)

data TxBody p
  = TxBody
  { inputs :: !(Set TxIn)
  , outputs :: ![TxOut]
  , fees :: !Coin
  , update :: ![UpdatePayload p]
    -- ^ Update payload
  } deriving (Show, Generic)

deriving instance ( Typeable p
                  , HasTypeReps (UpdatePayload p)
                  ) => HasTypeReps (TxBody p)


instance ( Typeable p
         , Sized (UpdatePayload p)
         ) => Sized (Tx p) where
  costsList _
    =  [ (typeOf (undefined :: TxIn), 1)
       , (typeOf (undefined :: TxOut), 1)
       , (typeOf (undefined :: Coin), 1)
       ]
    ++ costsList (undefined :: UpdatePayload p)


data TRANSACTION p

instance ( Hashable p
         , HasSigningScheme p
         , STS (UPDATES p)
         ) => STS (TRANSACTION p) where

  type Environment (TRANSACTION p) = Env p

  type State (TRANSACTION p) = St p

  type Signal (TRANSACTION p) = Tx p

  data PredicateFailure (TRANSACTION p)
    = TxFailure (PredicateFailure (UPDATES p))
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
        trans @(UPDATES p) $
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


instance (STS (TRANSACTION p)) => Embed UTXO (TRANSACTION p) where
  wrapFailed = error "UTXO transition shouldn't fail (yet)"


instance (STS (UPDATES p), STS (TRANSACTION p)) => Embed (UPDATES p) (TRANSACTION p) where
  wrapFailed = TxFailure

instance ( STS (TRANSACTION p)
         , STS.Gen.HasTrace (UPDATES p) ()
         ) => STS.Gen.HasTrace (TRANSACTION p) () where

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
                  @(UPDATES p)
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
    mkTx <$> STS.Gen.shrinkSignal @(UPDATES p) @() (update body)
    where
      mkTx :: [UpdatePayload p] -> Tx p
      mkTx updatePayload = Tx { body = body', witnesses = [] }
        where
          body' = body { update = updatePayload }
