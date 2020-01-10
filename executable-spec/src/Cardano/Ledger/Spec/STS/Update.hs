{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update where

-- import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
--                      GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)
import           Data.Typeable (Typeable)

import qualified Test.QuickCheck as QC

import           Control.State.Transition.Trace (traceSignals, TraceOrder (OldestFirst))
import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen
import           Data.AbstractSize (HasTypeReps)
import           Ledger.Core (Slot, BlockCount)

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
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation
import qualified Cardano.Ledger.Spec.STS.Update.GenApproval as GenApproval
import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)
import           Cardano.Ledger.Spec.STS.Update.GenApproval (GENAPPROVAL)
import           Cardano.Ledger.Spec.STS.Update.Data (UP)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.State.ActiveSUs (ActiveSUs)
import           Cardano.Ledger.Spec.State.SubmittedSUs (SubmittedSUs)
import           Cardano.Ledger.Spec.State.WhenSubmittedSUs (WhenSubmittedSUs)
import           Cardano.Ledger.Spec.State.WhenRevealedSUs (WhenRevealedSUs)
import           Cardano.Ledger.Spec.State.RevealedSUs (RevealedSUs)
import           Cardano.Ledger.Spec.State.BallotSUs (BallotSUs)
import qualified Cardano.Ledger.Spec.Classes.IsSU as IsSU
import qualified Cardano.Ledger.Spec.Classes.IsSUCommit as IsSUCommit
import qualified Cardano.Ledger.Spec.Classes.IsVoteForSU as IsVoteForSU
import           Cardano.Ledger.Spec.Classes.HasSigningScheme ( Signature
                                                              , VKey
                                                              )
import           Cardano.Ledger.Spec.Classes.Hashable (Hash)

data UPDATE p

-- | As we incorporate more phases, like UP (or IMPLEMENTATION), we will be
-- adding more components to this environment.
--
-- See @Ideation.Env@ for more details on the meaning of each field.
data Env p
  = Env
    { k :: !BlockCount
    , currentSlot :: !Slot
    , asips :: !(ActiveSIPs p)
    , aUPs :: !(ActiveSUs (UP p) p)
    , participants :: !(Participants p)
    , stakeDist :: !(StakeDistribution p)
    , apprvsips :: !(ApprovedSIPs p)
    }
  deriving (Generic)

deriving instance ( Hashable p
                  , Show (IsSU.SUHash (UP p) p)
                  , HasSigningScheme p
                  ) => Show (Env p)

data St p
  = St
    { subsips :: !(SubmittedSIPs p)
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
    }
  deriving (Generic)
  -- deriving Semigroup via GenericSemigroup (St u p)
  -- deriving Monoid via GenericMonoid (St u p)

instance ( Hashable p
         , Ord (IsSUCommit.CommitSU (UP p) p)
         , Ord (IsSU.SUHash (UP p) p)
         ) => Semigroup (St p) where
  (<>) St{ subsips = sp1, subUPs = s1, wssips = wsp1, wsUPs = ws1, wrsips = wrp1
         , wrUPs = wr1, sipdb = spd1, updb = sd1, ballots = bp1, ballotUPs = b1
         , implementationSt = i1
         }
       St{ subsips = sp2, subUPs = s2, wssips = wsp2, wsUPs = ws2, wrsips = wrp2
         , wrUPs = wr2, sipdb = spd2, updb = sd2, ballots = bp2, ballotUPs = b2
         , implementationSt = i2
         }
     = St{ subsips = sp1 <> sp2, subUPs = s1 <> s2, wssips = wsp1 <> wsp2
         , wsUPs = ws1 <> ws2, wrsips = wrp1 <> wrp2, wrUPs = wr1 <> wr2
         , sipdb = spd1 <> spd2, updb = sd1 <> sd2, ballots = bp1 <> bp2
         , ballotUPs = b1 <> b2, implementationSt = i1 <> i2
         }

instance (Hashable p, Ord (IsSUCommit.CommitSU (UP p) p), Ord (IsSU.SUHash (UP p) p)) => Monoid (St p) where
  mempty = St{ subsips = mempty, subUPs = mempty, wssips = mempty, wsUPs = mempty, wrsips = mempty
             , wrUPs = mempty, sipdb = mempty, updb = mempty, ballots = mempty, ballotUPs = mempty
             , implementationSt = mempty
             }


-- deriving instance (Ord (IsSUCommit.CommitSU u p)) => Semigroup (St u p)

-- deriving instance (Ord (IsSUCommit.CommitSU u p)) => Monoid (St u p)

deriving instance ( Hashable p
                  , Show (IsSUCommit.CommitSU (UP p) p)
                  , Show (IsSU.SUHash (UP p) p)
                  , Show (IsSU.SU (UP p) p)
                  , HasSigningScheme p
                  ) => Show (St p)

data UpdatePayload p
  = Ideation (Ideation.IdeationPayload p)
  | Implementation Implementation.ImplementationPayload
  | Approval (GenApproval.SUPayload (UP p) p)
  deriving (Generic)

deriving instance ( Hashable p
                  , Show p
                  , Hashable (UP p)
                  , HasSigningScheme p
                  , HasSigningScheme (UP p)
                  , Show (Data.UPHash p)
                  , Show (IsSUCommit.SUCommit (UP p) p)
                  , Show (IsVoteForSU.IsVote (UP p) p)
                  , Show (IsSU.SU (UP p) p)
                  ) => Show (UpdatePayload p)

deriving instance ( Typeable p
                  , HasTypeReps (Ideation.IdeationPayload p)
                  , HasTypeReps (IsSUCommit.SUCommit (UP p) p)
                  , HasTypeReps p
                  , HasTypeReps (VKey p)
                  , HasTypeReps (Hash p (Data.UPData p))
                  , HasTypeReps (Hash p Data.SIPData)
                  , HasTypeReps (IsSU.SU (UP p) p)
                  , HasTypeReps
                      (Signature p (Data.SIPHash p, Data.Confidence, VKey p))
                  , HasTypeReps
                          (Signature p (Data.UPHash p, Data.Confidence, VKey p))
                  , HasTypeReps (IsSU.SUHash (UP p) p)
                  , HasTypeReps (IsVoteForSU.IsVote (UP p) p)
                  ) => HasTypeReps (UpdatePayload p)

instance ( Typeable p
         , HasTypeReps (Ideation.IdeationPayload p)
         , HasTypeReps (IsSUCommit.SUCommit (UP p) p)
         , HasTypeReps p
         , HasTypeReps (VKey p)
         , HasTypeReps (Hash p (Data.UPData p))
         , HasTypeReps (Hash p Data.SIPData)
         , HasTypeReps (IsSU.SU (UP p) p)
         , HasTypeReps (Signature p (Data.SIPHash p, Data.Confidence, VKey p))
         , HasTypeReps (Signature p (Data.UPHash p, Data.Confidence, VKey p))
         , HasTypeReps (IsSU.SUHash (UP p) p)
         , HasTypeReps (IsVoteForSU.IsVote (UP p) p)
         ) => Sized (UpdatePayload p) where
  costsList _
    =  costsList (undefined :: Ideation.IdeationPayload p)
    ++ costsList (undefined :: Implementation.ImplementationPayload)

deriving instance ( Eq (GENAPPROVAL (UP p) p)
                  , HasSigningScheme p
                  , Hashable p
                  , Eq (IsSUCommit.CommitSU (UP p) p)
                  , Eq (IsSU.SU (UP p) p)
                  , Eq (Data.UPHash p)
                  ) => Eq (PredicateFailure (UPDATE p))

deriving instance ( Hashable p
                  , Show (GENAPPROVAL (UP p) p)
                  , HasSigningScheme p
                  , Show (IsSUCommit.CommitSU (UP p) p)
                  , Show (IsSU.SU (UP p) p)
                  , Eq (IsSU.SU (UP p) p)
                  , Show (Data.UPHash p)
                  ) => Show (PredicateFailure (UPDATE p))

instance ( Hashable p
         , HasSigningScheme p
         , STS (IDEATION p)
         , Eq (GENAPPROVAL (UP p) p)
         , Show (GENAPPROVAL (UP p) p)
         , Embed (GENAPPROVAL (UP p) p) (UPDATE p)
         , Eq (IsSUCommit.CommitSU (UP p) p)
         , Eq (IsSU.SU (UP p) p)
         , Show (IsSUCommit.CommitSU (UP p) p)
         , Eq (Data.UPHash p)
         , Show (IsSU.SU (UP p) p)
         , Show (Data.UPHash p)
         ) => STS (UPDATE p) where

  type Environment (UPDATE p) = Env p

  type State (UPDATE p) = St p

  type Signal (UPDATE p) = UpdatePayload p

  data PredicateFailure (UPDATE p)
    = IdeationsFailure (PredicateFailure (IDEATION p))
    | ImplementationsFailure (PredicateFailure (IMPLEMENTATION p))
    | ApprovalFailure  (PredicateFailure (GENAPPROVAL (UP p) p))

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { k
                , currentSlot
                , asips
                , aUPs
                , participants
                , apprvsips
                , stakeDist
                }
          , st@St { subsips
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
          , update
          ) <- judgmentContext

      case update of
        Ideation ideationPayload ->
          do
            Ideation.St { Ideation.subsips = subsips'
                        , Ideation.wssips = wssips'
                        , Ideation.wrsips = wrsips'
                        , Ideation.sipdb = sipdb'
                        , Ideation.ballots = ballots'
                        } <-
              trans @(IDEATION p)
                $ TRC ( Ideation.Env { Ideation.k = k
                                     , Ideation.currentSlot = currentSlot
                                     , Ideation.asips = asips
                                     , Ideation.participants = participants
                                     , Ideation.stakeDist = stakeDist
                                     }
                      , Ideation.St { Ideation.subsips = subsips
                                    , Ideation.wssips = wssips
                                    , Ideation.wrsips = wrsips
                                    , Ideation.sipdb = sipdb
                                    , Ideation.ballots = ballots
                                    }
                      , ideationPayload
                      )
            pure $ st { subsips = subsips'
                      , wssips = wssips'
                      , wrsips = wrsips'
                      , sipdb = sipdb'
                      , ballots = ballots'
                      }

        Implementation implementationPayload ->
          do
            implementationSt' <-
              trans @(IMPLEMENTATION p) $
                TRC ( Implementation.Env
                        currentSlot
                        apprvsips
                    , implementationSt
                    , implementationPayload
                    )
            pure $ st { implementationSt = implementationSt' }

        Approval approvalPayload ->
          do
            GenApproval.St { GenApproval.subSUs = subUPs'
                           , GenApproval.wsSUs = wsUPs'
                           , GenApproval.wrSUs = wrUPs'
                           , GenApproval.sudb = updb'
                           , GenApproval.ballots = ballotUPs'
                           } <-
              trans @(GENAPPROVAL (UP p) p) $
                TRC ( GenApproval.Env { GenApproval.k = k
                                      , GenApproval.currentSlot = currentSlot
                                      , GenApproval.aSUs = aUPs
                                      , GenApproval.apprvsips = apprvsips
                                      , GenApproval.participants = participants
                                      , GenApproval.stakeDist = stakeDist
                                      }
                    , GenApproval.St { GenApproval.subSUs = subUPs
                                     , GenApproval.wsSUs = wsUPs
                                     , GenApproval.wrSUs = wrUPs
                                     , GenApproval.sudb = updb
                                     , GenApproval.ballots = ballotUPs
                                     }
                    , approvalPayload
                    )
            pure $ st { subUPs = subUPs'
                      , wsUPs = wsUPs'
                      , wrUPs = wrUPs'
                      , updb = updb'
                      , ballotUPs = ballotUPs'
                      }
    ]

instance (STS (IDEATION p), STS (UPDATE p)) => Embed (IDEATION p) (UPDATE p) where
  wrapFailed = IdeationsFailure

instance (STS (GENAPPROVAL (UP p) p), STS (UPDATE p)) => Embed (GENAPPROVAL (UP p) p) (UPDATE p) where
  wrapFailed = ApprovalFailure

instance (STS (UPDATE p)) => Embed (IMPLEMENTATION p) (UPDATE p) where
  wrapFailed = ImplementationsFailure

data UPDATES p

deriving instance ( Eq (GENAPPROVAL (UP p) p)
                  , HasSigningScheme p, Hashable p
                  , Eq (IsSUCommit.CommitSU (UP p) p)
                  , Show (IsSUCommit.CommitSU (UP p) p)
                  , Eq (IsSU.SU (UP p) p)
                  , Eq (Data.UPHash p)
                  ) => Eq (PredicateFailure (UPDATES p))

deriving instance ( Show (GENAPPROVAL (UP p) p)
                  , Hashable p
                  , HasSigningScheme p
                  , Eq (IsSU.SU (UP p) p)
                  , Show (IsSUCommit.CommitSU (UP p) p)
                  , Show (IsSU.SU (UP p) p)
                  , Show (Data.UPHash p)
                  ) => Show (PredicateFailure (UPDATES p))

instance ( Hashable p
         , HasSigningScheme p
         , STS (UPDATE p)
         , Eq (GENAPPROVAL (UP p) p)
         , Show (GENAPPROVAL (UP p) p)
         , Show (IsSUCommit.CommitSU (UP p) p)
         , Eq (IsSUCommit.CommitSU (UP p) p)
         , Eq (IsSU.SU (UP p) p)
         , Eq (Data.UPHash p)
         , Show (IsSU.SU (UP p) p)
         , Show (Data.UPHash p)
         ) => STS (UPDATES p) where

  type Environment (UPDATES p) = Environment (UPDATE p)

  type State (UPDATES p) = State (UPDATE p)

  type Signal (UPDATES p) = [Signal (UPDATE p)]

  data PredicateFailure (UPDATES p)
    = UpdateFailure (PredicateFailure (UPDATE p))

  initialRules = []

  transitionRules = [
    do
      TRC (env, st, updates) <- judgmentContext
      case updates of
        [] -> pure $! st
        (update:updates') ->
          do
            st' <- trans @(UPDATE p) $ TRC (env, st, update)
            trans @(UPDATES p) $ TRC (env, st', updates')
    ]


instance (STS (UPDATE p), STS (UPDATES p)) => Embed (UPDATE p) (UPDATES p) where
  wrapFailed = UpdateFailure

--------------------------------------------------------------------------------
-- Trace generators
--------------------------------------------------------------------------------

instance ( STS (UPDATES p)
         , STS.Gen.HasTrace (UPDATE p) a
         ) => STS.Gen.HasTrace (UPDATES p) a where

  envGen traceGenEnv = STS.Gen.envGen @(UPDATE p) traceGenEnv

  sigGen traceGenEnv env st
    =   traceSignals OldestFirst
    <$> STS.Gen.traceFrom @(UPDATE p) 10 traceGenEnv env st
    -- We need to determine what is a realistic number of update
    -- transactions to be expected in a block.

  shrinkSignal =
    QC.shrinkList (STS.Gen.shrinkSignal @(UPDATE p) @a)

instance ( Hashable p
         , STS (UPDATE p)
         , STS.Gen.HasTrace (IDEATION p) ()
         , STS.Gen.HasTrace (GENAPPROVAL (UP p) p) ()
         ) => STS.Gen.HasTrace (UPDATE p) () where

  envGen traceGenEnv = do
    env <- STS.Gen.envGen @(IDEATION p) traceGenEnv
    envAppr <- STS.Gen.envGen @(GENAPPROVAL (UP p) p) traceGenEnv
    pure $!
      Env { k = Ideation.k env
          , currentSlot = Ideation.currentSlot env
          , asips = Ideation.asips env
          , aUPs = GenApproval.aSUs envAppr
          , participants = Ideation.participants env
          , stakeDist = Ideation.stakeDist env
          , apprvsips = mempty
          }

  sigGen
    ()
    Env { k, currentSlot, asips, participants, stakeDist }
    St { subsips, wssips, wrsips, sipdb, ballots }
    = do
    ideationPayload <-
      STS.Gen.sigGen
        @(IDEATION p)
        ()
        Ideation.Env { Ideation.k = k
                     , Ideation.currentSlot = currentSlot
                     , Ideation.asips = asips
                     , Ideation.participants = participants
                     , Ideation.stakeDist = stakeDist
                     }
        Ideation.St { Ideation.subsips = subsips
                    , Ideation.wssips = wssips
                    , Ideation.wrsips = wrsips
                    , Ideation.ballots = ballots
                    , Ideation.sipdb = sipdb
                    }
    pure $! Ideation ideationPayload

  shrinkSignal (Ideation ideationPayload) =
    Ideation <$> STS.Gen.shrinkSignal @(IDEATION p) @() ideationPayload
  shrinkSignal (Implementation _) = error "Shrinking of IMPLEMENTATION signals is not defined yet."
  shrinkSignal (Approval approvalPayload) =
    Approval <$> STS.Gen.shrinkSignal @(GENAPPROVAL (UP p) p) @() approvalPayload

