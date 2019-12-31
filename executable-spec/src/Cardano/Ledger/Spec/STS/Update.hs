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

import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
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
import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)


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
    , participants :: !(Participants p)
    , stakeDist :: !(StakeDistribution p)
    , apprvsips :: !(ApprovedSIPs p)
    }
  deriving (Show, Generic)


data St p
  = St
    { subsips :: !(SubmittedSIPs p)
    , wssips :: !(WhenSubmittedSIPs p)
    , wrsips :: !(WhenRevealedSIPs p)
    , sipdb :: !(RevealedSIPs p)
    , ballots :: !(Ballot p)
    , implementationSt :: State (IMPLEMENTATION p)
    }
  deriving (Show, Generic)
  deriving Semigroup via GenericSemigroup (St p)
  deriving Monoid via GenericMonoid (St p)


data UpdatePayload p
  = Ideation (Ideation.IdeationPayload p)
  | Implementation Implementation.ImplementationPayload
  deriving (Show, Generic)

deriving instance ( Typeable p
                  , HasTypeReps (Ideation.IdeationPayload p)
                  ) => HasTypeReps (UpdatePayload p)

instance ( Typeable p
         , HasTypeReps (Ideation.IdeationPayload p)
         ) => Sized (UpdatePayload p) where
  costsList _
    =  costsList (undefined :: Ideation.IdeationPayload p)
    ++ costsList (undefined :: Implementation.ImplementationPayload)

instance ( Hashable p
         , HasSigningScheme p
         , STS (IDEATION p)
         ) => STS (UPDATE p) where

  type Environment (UPDATE p) = Env p

  type State (UPDATE p) = St p

  type Signal (UPDATE p) = UpdatePayload p

  data PredicateFailure (UPDATE p)
    = IdeationsFailure (PredicateFailure (IDEATION p))
    | ImplementationsFailure (PredicateFailure (IMPLEMENTATION p))
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { k
                , currentSlot
                , asips
                , participants
                , apprvsips
                , stakeDist
                }
          , st@St { subsips
                  , wssips
                  , wrsips
                  , sipdb
                  , ballots
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

    ]

instance (STS (IDEATION p), STS (UPDATE p)) => Embed (IDEATION p) (UPDATE p) where
  wrapFailed = IdeationsFailure

instance (STS (UPDATE p)) => Embed (IMPLEMENTATION p) (UPDATE p) where
  wrapFailed = ImplementationsFailure

data UPDATES p

instance ( Hashable p
         , HasSigningScheme p
         , STS (UPDATE p)
         ) => STS (UPDATES p) where

  type Environment (UPDATES p) = Environment (UPDATE p)

  type State (UPDATES p) = State (UPDATE p)

  type Signal (UPDATES p) = [Signal (UPDATE p)]

  data PredicateFailure (UPDATES p)
    = UpdateFailure (PredicateFailure (UPDATE p))
    deriving (Eq, Show)

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
         ) => STS.Gen.HasTrace (UPDATE p) () where

  envGen traceGenEnv = do
    env <- STS.Gen.envGen @(IDEATION p) traceGenEnv
    pure $!
      Env { k = Ideation.k env
          , currentSlot = Ideation.currentSlot env
          , asips = Ideation.asips env
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
