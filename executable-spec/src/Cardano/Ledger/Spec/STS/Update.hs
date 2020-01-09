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

data UPDATE u p

-- | As we incorporate more phases, like UP (or IMPLEMENTATION), we will be
-- adding more components to this environment.
--
-- See @Ideation.Env@ for more details on the meaning of each field.
data Env u p
  = Env
    { k :: !BlockCount
    , currentSlot :: !Slot
    , asips :: !(ActiveSIPs p)
    , aSUs :: !(ActiveSUs u p)
    , participants :: !(Participants p)
    , stakeDist :: !(StakeDistribution p)
    , apprvsips :: !(ApprovedSIPs p)
    }
  deriving (Generic)

deriving instance ( Hashable p
                  , Show (IsSU.SUHash u p)
                  , HasSigningScheme p
                  ) => Show (Env u p)

data St u p
  = St
    { subsips :: !(SubmittedSIPs p)
    , subSUs :: !(SubmittedSUs u p)
    , wssips :: !(WhenSubmittedSIPs p)
    , wsSUs :: !(WhenSubmittedSUs u p)
    , wrsips :: !(WhenRevealedSIPs p)
    , wrSUs :: !(WhenRevealedSUs u p)
    , sipdb :: !(RevealedSIPs p)
    , sudb :: !(RevealedSUs u p)
    , ballots :: !(Ballot p)
    , ballotSUs :: !(BallotSUs u p)
    , implementationSt :: State (IMPLEMENTATION p)
    }
  deriving (Generic)
  -- deriving Semigroup via GenericSemigroup (St u p)
  -- deriving Monoid via GenericMonoid (St u p)

instance ( Hashable p
         , Ord (IsSUCommit.CommitSU u p)
         , Ord (IsSU.SUHash u p)
         ) => Semigroup (St u p) where
  (<>) St{ subsips = sp1, subSUs = s1, wssips = wsp1, wsSUs = ws1, wrsips = wrp1
         , wrSUs = wr1, sipdb = spd1, sudb = sd1, ballots = bp1, ballotSUs = b1
         , implementationSt = i1
         }
       St{ subsips = sp2, subSUs = s2, wssips = wsp2, wsSUs = ws2, wrsips = wrp2
         , wrSUs = wr2, sipdb = spd2, sudb = sd2, ballots = bp2, ballotSUs = b2
         , implementationSt = i2
         }
     = St{ subsips = sp1 <> sp2, subSUs = s1 <> s2, wssips = wsp1 <> wsp2
         , wsSUs = ws1 <> ws2, wrsips = wrp1 <> wrp2, wrSUs = wr1 <> wr2
         , sipdb = spd1 <> spd2, sudb = sd1 <> sd2, ballots = bp1 <> bp2
         , ballotSUs = b1 <> b2, implementationSt = i1 <> i2
         }

instance (Hashable p, Ord (IsSUCommit.CommitSU u p), Ord (IsSU.SUHash u p)) => Monoid (St u p) where
  mempty = St{ subsips = mempty, subSUs = mempty, wssips = mempty, wsSUs = mempty, wrsips = mempty
             , wrSUs = mempty, sipdb = mempty, sudb = mempty, ballots = mempty, ballotSUs = mempty
             , implementationSt = mempty
             }


-- deriving instance (Ord (IsSUCommit.CommitSU u p)) => Semigroup (St u p)

-- deriving instance (Ord (IsSUCommit.CommitSU u p)) => Monoid (St u p)

deriving instance ( Hashable p
                  , Show (IsSUCommit.CommitSU u p)
                  , Show (IsSU.SUHash u p)
                  , Show (IsSU.SU u p)
                  , HasSigningScheme p
                  ) => Show (St u p)

data UpdatePayload u p
  = Ideation (Ideation.IdeationPayload p)
  | Implementation Implementation.ImplementationPayload
  | Approval (GenApproval.SUPayload u p)
  deriving (Generic)

deriving instance ( Hashable p
                  , Hashable u
                  , Show p
                  , Hashable (Data.UP p)
                  , HasSigningScheme p
                  , HasSigningScheme u
                  , HasSigningScheme (Data.UP p)
                  , Show (IsSUCommit.SUCommit (Data.UP p) p)
                  , Show (IsSU.SU (Data.UP p) p)
                  , Show (Data.UPHash p)
                  , Show (IsSUCommit.SUCommit u p)
                  , Show (IsVoteForSU.IsVote u p)
                  , Show (IsSU.SU u p)
                  ) => Show (UpdatePayload u p)

deriving instance ( Typeable p
                  , Typeable u
                  , HasTypeReps (Ideation.IdeationPayload p)
                  , HasTypeReps (IsSUCommit.SUCommit (Data.UP p) p)
                  , HasTypeReps p
                  , HasTypeReps u
                  , HasTypeReps (VKey p)
                  , HasTypeReps (Hash p (Data.UPData p))
                  , HasTypeReps (Hash p Data.SIPData)
                  , HasTypeReps (IsSU.SU (Data.UP p) p)
                  , HasTypeReps
                      (Signature p (Data.SIPHash p, Data.Confidence, VKey p))
                  , HasTypeReps
                          (Signature p (Data.UPHash p, Data.Confidence, VKey p))
                  , HasTypeReps (IsSU.SUHash u p)
                  , HasTypeReps (IsSUCommit.SUCommit u p)
                  , HasTypeReps (IsVoteForSU.IsVote u p)
                  , HasTypeReps (IsSU.SU u p)
                  ) => HasTypeReps (UpdatePayload u p)

instance ( Typeable p
         , Typeable u
         , HasTypeReps (Ideation.IdeationPayload p)
         , HasTypeReps (IsSUCommit.SUCommit (Data.UP p) p)
         , HasTypeReps p
         , HasTypeReps u
         , HasTypeReps (VKey p)
         , HasTypeReps (Hash p (Data.UPData p))
         , HasTypeReps (Hash p Data.SIPData)
         , HasTypeReps (IsSU.SU (Data.UP p) p)
         , HasTypeReps (Signature p (Data.SIPHash p, Data.Confidence, VKey p))
         , HasTypeReps (Signature p (Data.UPHash p, Data.Confidence, VKey p))
         , HasTypeReps (IsSU.SUHash u p)
         , HasTypeReps (IsSUCommit.SUCommit u p)
         , HasTypeReps (IsVoteForSU.IsVote u p)
         , HasTypeReps (IsSU.SU u p)
         ) => Sized (UpdatePayload u p) where
  costsList _
    =  costsList (undefined :: Ideation.IdeationPayload p)
    ++ costsList (undefined :: Implementation.ImplementationPayload)

deriving instance ( Eq (GENAPPROVAL (Data.UP p) p)
                  , HasSigningScheme p
                  , Hashable p
                  ) => Eq (PredicateFailure (UPDATE u p))

deriving instance ( Hashable p, Show (GENAPPROVAL (Data.UP p) p), HasSigningScheme p
                ) => Show (PredicateFailure (UPDATE u p))

instance ( Hashable p
         , HasSigningScheme p
         , STS (IDEATION p)
         , Eq (GENAPPROVAL (Data.UP p) p)
         , Show (GENAPPROVAL (Data.UP p) p)
         , Embed (GENAPPROVAL u p) (UPDATE u p)
         ) => STS (UPDATE u p) where

  type Environment (UPDATE u p) = Env u p

  type State (UPDATE u p) = St u p

  type Signal (UPDATE u p) = UpdatePayload u p

  data PredicateFailure (UPDATE u p)
    = IdeationsFailure (PredicateFailure (IDEATION p))
    | ImplementationsFailure (PredicateFailure (IMPLEMENTATION p))
    | ApprovalFailure (GENAPPROVAL (Data.UP p) p)

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { k
                , currentSlot
                , asips
                , aSUs
                , participants
                , apprvsips
                , stakeDist
                }
          , st@St { subsips
                  , subSUs
                  , wssips
                  , wsSUs
                  , wrsips
                  , wrSUs
                  , sipdb
                  , sudb
                  , ballots
                  , ballotSUs
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
            GenApproval.St { GenApproval.subSUs = subSUs'
                           , GenApproval.wsSUs = wsSUs'
                           , GenApproval.wrSUs = wrSUs'
                           , GenApproval.sudb = sudb'
                           , GenApproval.ballots = ballotSUs'
                           } <-
              trans @(GENAPPROVAL u p) $
                TRC ( GenApproval.Env { GenApproval.k = k
                                      , GenApproval.currentSlot = currentSlot
                                      , GenApproval.aSUs = aSUs
                                      , GenApproval.apprvsips = apprvsips
                                      , GenApproval.participants = participants
                                      , GenApproval.stakeDist = stakeDist
                                      }
                    , GenApproval.St { GenApproval.subSUs = subSUs
                                     , GenApproval.wsSUs = wsSUs
                                     , GenApproval.wrSUs = wrSUs
                                     , GenApproval.sudb = sudb
                                     , GenApproval.ballots = ballotSUs
                                     }
                    , approvalPayload
                    )
            pure $ st { subSUs = subSUs'
                      , wsSUs = wsSUs'
                      , wrSUs = wrSUs'
                      , sudb = sudb'
                      , ballotSUs = ballotSUs'
                      }
    ]

instance (STS (IDEATION p), STS (UPDATE u p)) => Embed (IDEATION p) (UPDATE u p) where
  wrapFailed = IdeationsFailure

instance (STS (UPDATE u p)) => Embed (IMPLEMENTATION p) (UPDATE u p) where
  wrapFailed = ImplementationsFailure

data UPDATES u p

deriving instance ( Eq (GENAPPROVAL (Data.UP p) p), HasSigningScheme p, Hashable p
                ) => Eq (PredicateFailure (UPDATES u p))

deriving instance ( Show (GENAPPROVAL (Data.UP p) p), Hashable p, HasSigningScheme p
                ) => Show (PredicateFailure (UPDATES u p))

instance ( Hashable p
         , HasSigningScheme p
         , STS (UPDATE u p)
         , Eq (GENAPPROVAL (Data.UP p) p)
         , Show (GENAPPROVAL (Data.UP p) p)
         ) => STS (UPDATES u p) where

  type Environment (UPDATES u p) = Environment (UPDATE u p)

  type State (UPDATES u p) = State (UPDATE u p)

  type Signal (UPDATES u p) = [Signal (UPDATE u p)]

  data PredicateFailure (UPDATES u p)
    = UpdateFailure (PredicateFailure (UPDATE u p))

  initialRules = []

  transitionRules = [
    do
      TRC (env, st, updates) <- judgmentContext
      case updates of
        [] -> pure $! st
        (update:updates') ->
          do
            st' <- trans @(UPDATE u p) $ TRC (env, st, update)
            trans @(UPDATES u p) $ TRC (env, st', updates')
    ]


instance (STS (UPDATE u p), STS (UPDATES u p)) => Embed (UPDATE u p) (UPDATES u p) where
  wrapFailed = UpdateFailure

--------------------------------------------------------------------------------
-- Trace generators
--------------------------------------------------------------------------------

instance ( STS (UPDATES u p)
         , STS.Gen.HasTrace (UPDATE u p) a
         ) => STS.Gen.HasTrace (UPDATES u p) a where

  envGen traceGenEnv = STS.Gen.envGen @(UPDATE u p) traceGenEnv

  sigGen traceGenEnv env st
    =   traceSignals OldestFirst
    <$> STS.Gen.traceFrom @(UPDATE u p) 10 traceGenEnv env st
    -- We need to determine what is a realistic number of update
    -- transactions to be expected in a block.

  shrinkSignal =
    QC.shrinkList (STS.Gen.shrinkSignal @(UPDATE u p) @a)

instance ( Hashable p
         , STS (UPDATE u p)
         , STS.Gen.HasTrace (IDEATION p) ()
         , STS.Gen.HasTrace (GENAPPROVAL u p) ()
         ) => STS.Gen.HasTrace (UPDATE u p) () where

  envGen traceGenEnv = do
    env <- STS.Gen.envGen @(IDEATION p) traceGenEnv
    envAppr <- STS.Gen.envGen @(GENAPPROVAL u p) traceGenEnv
    pure $!
      Env { k = Ideation.k env
          , currentSlot = Ideation.currentSlot env
          , asips = Ideation.asips env
          , aSUs = GenApproval.aSUs envAppr
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
    Approval <$> STS.Gen.shrinkSignal @(GENAPPROVAL u p) @() approvalPayload

