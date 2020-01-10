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
{-# LANGUAGE FlexibleInstances #-}

module Cardano.Ledger.Spec.STS.Chain.Transaction where

import           Control.Exception (assert)
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
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.State.SubmittedSIPs (SubmittedSIPs)
import           Cardano.Ledger.Spec.State.ActiveSUs (ActiveSUs)
import           Cardano.Ledger.Spec.State.BallotSUs (BallotSUs)
import           Cardano.Ledger.Spec.State.WhenRevealedSUs (WhenRevealedSUs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSUs (WhenSubmittedSUs)
import           Cardano.Ledger.Spec.State.RevealedSUs (RevealedSUs)
import           Cardano.Ledger.Spec.State.SubmittedSUs (SubmittedSUs)
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (TxIn, TxOut, Coin (Coin), Witness)
import           Cardano.Ledger.Spec.STS.Update (UpdatePayload, UPDATES)
import qualified Cardano.Ledger.Spec.STS.Update as Update
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)
import           Cardano.Ledger.Spec.STS.Dummy.UTxO (UTXO)
import qualified Cardano.Ledger.Spec.STS.Dummy.UTxO as UTxO
import           Cardano.Ledger.Spec.STS.Update.Data (UP)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.Classes.IsSU as IsSU
import qualified Cardano.Ledger.Spec.Classes.IsSUCommit as IsSUCommit
import           Cardano.Ledger.Spec.STS.Update.GenApproval (GENAPPROVAL)


-- | Environment of the TRANSACTION STS
data Env p =
  Env { k :: !BlockCount
      , currentSlot :: !Slot
      , asips :: !(ActiveSIPs p)
      , aUPs :: !(ActiveSUs (UP p) p)
      , participants :: !(Participants p)
      , stakeDist :: !(StakeDistribution p)
      , apprvsips :: !(ApprovedSIPs p)
      , utxoEnv :: !(Environment UTXO)
      }
  deriving (Generic)

deriving instance ( Hashable p
                  , HasSigningScheme p
                  , Show (Data.UPHash p)
                  ) => Show (Env p)

-- | State of the TRANSACTION STS
data St p =
  St { subsips :: !(SubmittedSIPs p)
     , subUPs :: !(SubmittedSUs (UP p) p)
     , wssips :: !(WhenSubmittedSIPs p)
     , wsUPs :: !(WhenSubmittedSUs (UP p) p)
     , wrsips :: !(WhenRevealedSIPs p)
     , wrUPs :: !(WhenRevealedSUs (UP p) p)
     , sipdb :: !(RevealedSIPs p)
     , updb :: !(RevealedSUs (UP p) p)
     , ballots :: !(Ballot p)
     , ballotUPs :: !(BallotSUs (UP p) p)
     , implementationSt :: State (IMPLEMENTATION p)
     , utxoSt :: State UTXO
     }
  deriving (Generic)

deriving instance ( Hashable p
                  , HasSigningScheme p
                  , Show (IsSUCommit.CommitSU (UP p) p)
                  , Show (IsSU.SU (UP p) p)
                  , Show (Data.UPHash p)
                  ) => Show (St p)

instance ( Hashable p
         , Ord (Data.UPHash p)
         , Ord (IsSUCommit.CommitSU (UP p) p)
         ) => Semigroup (St p) where
  (<>) St{ subsips = ss1, subUPs = su1, wssips = wsp1, wsUPs = wsu1
         , wrsips = wrs1, wrUPs = wru1, sipdb = sd1, updb = ud1
         , ballots = b1, ballotUPs = bu1, implementationSt = i1, utxoSt = u1 }
       St{ subsips = ss2, subUPs = su2, wssips = wsp2, wsUPs = wsu2
         , wrsips = wrs2, wrUPs = wru2, sipdb = sd2, updb = ud2
         , ballots = b2, ballotUPs = bu2, implementationSt = i2, utxoSt = u2 }
     = St{ subsips = ss1 <> ss2, subUPs = su1 <> su2, wssips = wsp1 <> wsp2
         , wsUPs = wsu1 <> wsu2, wrsips = wrs1 <> wrs2, wrUPs = wru1 <> wru2
         , sipdb = sd1 <> sd2, updb = ud1 <> ud2, ballots = b1 <> b2
         , ballotUPs = bu1 <> bu2, implementationSt = i1 <> i2
         , utxoSt = u1 <> u2 }

instance ( Hashable p
         , Ord (Data.UPHash p)
         , Ord (IsSUCommit.CommitSU (UP p) p)
         ) => Monoid (St p) where
  mempty = St{ subsips = mempty, subUPs = mempty, wssips = mempty, wsUPs = mempty
         , wrsips = mempty, wrUPs = mempty, sipdb = mempty, updb = mempty
         , ballots = mempty, ballotUPs = mempty
         , implementationSt = mempty, utxoSt = mempty }

-- | Transactions contained in a block.
data Tx p
  = Tx
  { body :: TxBody p
  , witnesses :: ![Witness]
  }
  deriving (Generic)

deriving instance ( Hashable p
                  , Hashable (UP p)
                  , HasSigningScheme p
                  , HasSigningScheme (UP p)
                  , Show p
                  , Show (IsSUCommit.SUCommit (UP p) p)
                  , Show (IsSU.SU (UP p) p)
                  , Show (Data.UPHash p)
                  ) => Show (Tx p)

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
  } deriving (Generic)

deriving instance ( Hashable p
                  , Hashable (UP p)
                  , HasSigningScheme p
                  , HasSigningScheme (UP p)
                  , Show p
                  , Show (IsSUCommit.SUCommit (UP p) p)
                  , Show (IsSU.SU (UP p) p)
                  , Show (Data.UPHash p)
                  ) => Show (TxBody p)

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


deriving instance ( Hashable p
                  , HasSigningScheme p
                  , Eq (GENAPPROVAL (UP p) p)
                  ) => Eq (PredicateFailure (TRANSACTION p))
deriving instance ( Hashable p
                  , HasSigningScheme p
                  , Show (GENAPPROVAL (UP p) p)
                  ) => Show (PredicateFailure (TRANSACTION p))

data TRANSACTION p

instance ( Hashable p
         , HasSigningScheme p
         , STS (UPDATES p)
         , Eq (GENAPPROVAL (UP p) p)
         , Show (GENAPPROVAL (UP p) p)
         ) => STS (TRANSACTION p) where

  type Environment (TRANSACTION p) = Env p

  type State (TRANSACTION p) = St p

  type Signal (TRANSACTION p) = Tx p

  data PredicateFailure (TRANSACTION p)
    = TxFailure (PredicateFailure (UPDATES p))

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { k
                , currentSlot
                , asips
                , aUPs
                , participants
                , stakeDist
                , apprvsips
                , utxoEnv
                }
          , St { subsips
               , subUPs
               , wssips
               , wsUPs
               , wrsips
               , wrUPs
               , sipdb
               , updb
               , ballots
               , ballotUPs
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
                , Update.subUPs = subUPs'
                , Update.wssips = wssips'
                , Update.wsUPs = wsUPs'
                , Update.wrsips = wrsips'
                , Update.wrUPs = wrUPs'
                , Update.sipdb = sipdb'
                , Update.updb = updb'
                , Update.ballots = ballots'
                , Update.ballotUPs = ballotUPs'
                , Update.implementationSt = implementationSt'
                } <-
        trans @(UPDATES p) $
          TRC ( Update.Env { Update.k = k
                           , Update.currentSlot = currentSlot
                           , Update.asips = asips
                           , Update.aUPs = aUPs
                           , Update.participants = participants
                           , Update.stakeDist = stakeDist
                           , Update.apprvsips = apprvsips
                           }
              , Update.St { Update.subsips = subsips
                          , Update.subUPs = subUPs
                          , Update.wssips = wssips
                          , Update.wsUPs = wsUPs
                          , Update.wrsips = wrsips
                          , Update.wrUPs = wrUPs
                          , Update.sipdb = sipdb
                          , Update.updb = updb
                          , Update.ballots = ballots
                          , Update.ballotUPs = ballotUPs
                          , Update.implementationSt = implementationSt
                          }
              , update
              )
      pure $ St { subsips = subsips'
                , subUPs = subUPs'
                , wssips = wssips'
                , wsUPs = wsUPs'
                , wrsips = wrsips'
                , wrUPs = wrUPs'
                , sipdb = sipdb'
                , updb = updb'
                , ballots = ballots'
                , ballotUPs = ballotUPs'
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
         , aUPs
         , participants
         , stakeDist
         , apprvsips
         }
    )
    (St { subsips
        , subUPs
        , wssips
        , wsUPs
        , wrsips
        , wrUPs
        , sipdb
        , updb
        , ballots
        , ballotUPs
        , implementationSt
        }
    )
    = do
    someUpdatePayload <-
      QC.frequency
        [ (70, pure $! []) -- We don't generate payload in 7/10 of the cases.
        , (30, do
              someUpdatePayload <-
                STS.Gen.sigGen
                  @(UPDATES p)
                  ()
                  Update.Env { Update.k = k
                             , Update.currentSlot = currentSlot
                             , Update.asips = asips
                             , Update.aUPs = aUPs
                             , Update.participants = participants
                             , Update.stakeDist = stakeDist
                             , Update.apprvsips = apprvsips
                             }
                  Update.St { Update.subsips = subsips
                            , Update.subUPs = subUPs
                            , Update.wssips = wssips
                            , Update.wsUPs = wsUPs
                            , Update.wrsips = wrsips
                            , Update.wrUPs = wrUPs
                            , Update.sipdb = sipdb
                            , Update.updb = updb
                            , Update.ballots = ballots
                            , Update.ballotUPs = ballotUPs
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
