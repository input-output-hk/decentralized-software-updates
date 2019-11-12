{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update where

import           Data.Bimap (Bimap)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)
import           Data.Typeable (Typeable)
import           Data.Map.Strict (Map)
import           Data.Set as Set (Set)
import qualified Data.Set as Set

import qualified Test.QuickCheck as QC

import           Cardano.Binary (ToCBOR)
import           Cardano.Crypto.Hash (Hash, HashAlgorithm)
import           Cardano.Crypto.DSIGN.Class (SignedDSIGN, VerKeyDSIGN, SignKeyDSIGN)
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)

import           Control.State.Transition.Trace (traceSignals, TraceOrder (OldestFirst))
import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as STS.Gen
import           Data.AbstractSize (HasTypeReps)
import           Ledger.Core (Slot, BlockCount)

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation
import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)


data UPDATE hashAlgo dsignAlgo


-- | As we incorporate more phases, like UP (or IMPLEMENTATION), we will be
-- adding more components to this environment.
--
-- See @Ideation.Env@ for more details on the meaning of each field.
data Env hashAlgo dsignAlgo
  = Env
    { k :: !BlockCount
    , currentSlot :: !Slot
    , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
    , participants :: Bimap (VerKeyDSIGN dsignAlgo) (SignKeyDSIGN dsignAlgo)
    , apprvsips :: !(Set (Data.SIPHash hashAlgo))
    }
  deriving (Generic)

deriving instance
  (Eq (VerKeyDSIGN dsignAlgo), Eq (SignKeyDSIGN dsignAlgo)) => Eq (Env hashAlgo dsignAlgo)

deriving instance
  (Show (VerKeyDSIGN dsignAlgo), Show (SignKeyDSIGN dsignAlgo)) => Show (Env hashAlgo dsignAlgo)

data St hashAlgo dsignAlgo
  = St
    { subsips :: !(Map (Data.Commit hashAlgo dsignAlgo) (Data.SIP hashAlgo dsignAlgo))
    , wssips :: !(Map (Data.Commit hashAlgo dsignAlgo) Slot)
    , wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
    , sipdb :: !(Map (Data.SIPHash hashAlgo) (Data.SIP hashAlgo dsignAlgo))
    , ballots :: !(Map (Data.SIPHash hashAlgo) (Map (VerKeyDSIGN dsignAlgo) Data.Confidence))
    , implementationSt :: State (IMPLEMENTATION hashAlgo)
    }
  deriving (Generic)
  deriving Semigroup via GenericSemigroup (St hashAlgo dsignAlgo)
  deriving Monoid via GenericMonoid (St hashAlgo dsignAlgo)

deriving instance
  (Eq (VerKeyDSIGN dsignAlgo)) => Eq (St hashAlgo dsignAlgo)

deriving instance
  (Show (VerKeyDSIGN dsignAlgo)) => Show (St hashAlgo dsignAlgo)

data UpdatePayload hashAlgo dsignAlgo
  = Ideation (Data.IdeationPayload hashAlgo dsignAlgo)
  | Implementation Data.ImplementationPayload
  deriving (Eq, Show, Generic)

deriving instance ( Typeable hashAlgo
                  , Typeable dsignAlgo
                  , HasTypeReps hashAlgo
                  , HasTypeReps (Data.Commit hashAlgo dsignAlgo)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Hash hashAlgo Data.SIPData)
                  , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo dsignAlgo))
                  , HasTypeReps (VerKeyDSIGN dsignAlgo)
                  ) => HasTypeReps (UpdatePayload hashAlgo dsignAlgo)

instance ( Typeable hashAlgo
         , Typeable dsignAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo dsignAlgo)
         , HasTypeReps (SignedDSIGN dsignAlgo (Data.Commit hashAlgo dsignAlgo))
         , HasTypeReps (VerKeyDSIGN dsignAlgo)
         ) => Sized (UpdatePayload hashAlgo dsignAlgo) where
  costsList _
    =  costsList (undefined :: Data.IdeationPayload hashAlgo dsignAlgo)
    ++ costsList (undefined :: Data.ImplementationPayload)

instance ( HashAlgorithm hashAlgo
         , Typeable dsignAlgo
         , ToCBOR (VerKeyDSIGN dsignAlgo)
         , Show (VerKeyDSIGN dsignAlgo)
         , Show (SignKeyDSIGN dsignAlgo)
         , Ord (VerKeyDSIGN dsignAlgo)
         , Ord (SignKeyDSIGN dsignAlgo)
         ) => STS (UPDATE hashAlgo dsignAlgo) where

  type Environment (UPDATE hashAlgo dsignAlgo) = Env hashAlgo dsignAlgo

  type State (UPDATE hashAlgo dsignAlgo) = St hashAlgo dsignAlgo

  type Signal (UPDATE hashAlgo dsignAlgo) = UpdatePayload hashAlgo dsignAlgo

  data PredicateFailure (UPDATE hashAlgo dsignAlgo)
    = IdeationsFailure (PredicateFailure (IDEATION hashAlgo dsignAlgo))
    | ImplementationsFailure (PredicateFailure (IMPLEMENTATION hashAlgo))

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { k
                , currentSlot
                , asips
                , participants
                , apprvsips
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
              trans @(IDEATION hashAlgo dsignAlgo)
                $ TRC ( Ideation.Env { Ideation.k = k
                                     , Ideation.currentSlot = currentSlot
                                     , Ideation.asips = asips
                                     , Ideation.participants = participants
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
              trans @(IMPLEMENTATION hashAlgo) $
              TRC ( Implementation.Env
                      currentSlot
                      apprvsips
                  , implementationSt
                  , implementationPayload
                  )
            pure $ st { implementationSt = implementationSt' }

    ]

deriving instance
  (Eq (VerKeyDSIGN dsignAlgo)) => Eq (PredicateFailure (UPDATE hashAlgo dsignAlgo))

deriving instance
  (Show (VerKeyDSIGN dsignAlgo)) => Show (PredicateFailure (UPDATE hashAlgo dsignAlgo))

instance ( HashAlgorithm hashAlgo
         , Typeable dsignAlgo
         , ToCBOR (VerKeyDSIGN dsignAlgo)
         , Show (VerKeyDSIGN dsignAlgo)
         , Show (SignKeyDSIGN dsignAlgo)
         , Ord (VerKeyDSIGN dsignAlgo)
         , Ord (SignKeyDSIGN dsignAlgo)
         ) => Embed (IDEATION hashAlgo dsignAlgo) (UPDATE hashAlgo dsignAlgo) where
  wrapFailed = IdeationsFailure

instance ( HashAlgorithm hashAlgo
         , Typeable dsignAlgo
         , ToCBOR (VerKeyDSIGN dsignAlgo)
         , Show (VerKeyDSIGN dsignAlgo)
         , Show (SignKeyDSIGN dsignAlgo)
         , Ord (VerKeyDSIGN dsignAlgo)
         , Ord (SignKeyDSIGN dsignAlgo)
         ) => Embed (IMPLEMENTATION hashAlgo) (UPDATE hashAlgo dsignAlgo) where
  wrapFailed = ImplementationsFailure

data UPDATES hashAlgo dsignAlgo

instance ( Typeable dsignAlgo
         , HashAlgorithm hashAlgo
         , Eq (VerKeyDSIGN dsignAlgo)
         , Show (VerKeyDSIGN dsignAlgo)
         , ToCBOR (VerKeyDSIGN dsignAlgo)
         , Show (SignKeyDSIGN dsignAlgo)
         , Ord (VerKeyDSIGN dsignAlgo)
         , Ord (SignKeyDSIGN dsignAlgo)
         ) => STS (UPDATES hashAlgo dsignAlgo) where

  type Environment (UPDATES hashAlgo dsignAlgo) = Environment (UPDATE hashAlgo dsignAlgo)

  type State (UPDATES hashAlgo dsignAlgo) = State (UPDATE hashAlgo dsignAlgo)

  type Signal (UPDATES hashAlgo dsignAlgo) = [Signal (UPDATE hashAlgo dsignAlgo)]

  data PredicateFailure (UPDATES hashAlgo dsignAlgo)
    = UpdateFailure (PredicateFailure (UPDATE hashAlgo dsignAlgo))

  initialRules = []

  transitionRules = [
    do
      TRC (env, st, updates) <- judgmentContext
      case updates of
        [] -> pure $! st
        (update:updates') ->
          do
            st' <- trans @(UPDATE hashAlgo dsignAlgo) $ TRC (env, st, update)
            trans @(UPDATES hashAlgo dsignAlgo) $ TRC (env, st', updates')
    ]

deriving instance
  (Eq (VerKeyDSIGN dsignAlgo)) => Eq (PredicateFailure (UPDATES hashAlgo dsignAlgo))

deriving instance
  (Show (VerKeyDSIGN dsignAlgo)) => Show (PredicateFailure (UPDATES hashAlgo dsignAlgo))

instance ( Typeable dsignAlgo
         , HashAlgorithm hashAlgo
         , Eq (VerKeyDSIGN dsignAlgo)
         , Show (VerKeyDSIGN dsignAlgo)
         , ToCBOR (VerKeyDSIGN dsignAlgo)
         , Show (SignKeyDSIGN dsignAlgo)
         , Ord (VerKeyDSIGN dsignAlgo)
         , Ord (SignKeyDSIGN dsignAlgo)
         ) => Embed (UPDATE hashAlgo dsignAlgo) (UPDATES hashAlgo dsignAlgo) where
  wrapFailed = UpdateFailure

--------------------------------------------------------------------------------
-- Trace generators
--------------------------------------------------------------------------------

instance HashAlgorithm hashAlgo => STS.Gen.HasTrace (UPDATES hashAlgo MockDSIGN) () where

  envGen traceGenEnv = STS.Gen.envGen @(UPDATE hashAlgo MockDSIGN) traceGenEnv

  sigGen _traceGenEnv env st
    =   traceSignals OldestFirst
    <$> STS.Gen.traceFrom @(UPDATE hashAlgo MockDSIGN) 10 () env st
    -- We need to determine what is a realistic number of update
    -- transactions to be expected in a block.

  shrinkSignal =
    QC.shrinkList (STS.Gen.shrinkSignal @(UPDATE hashAlgo MockDSIGN) @())

instance HashAlgorithm hashAlgo => STS.Gen.HasTrace (UPDATE hashAlgo MockDSIGN) () where

  envGen traceGenEnv = do
    env <- STS.Gen.envGen @(IDEATION hashAlgo MockDSIGN) traceGenEnv
    pure $!
      Env { k = Ideation.k env
          , currentSlot = Ideation.currentSlot env
          , asips = Ideation.asips env
          , participants = Ideation.participants env
          , apprvsips = Set.empty
          }

  sigGen
    ()
    Env { k, currentSlot, asips, participants }
    St { subsips, wssips, wrsips, sipdb, ballots }
    = do
    ideationPayload <-
      STS.Gen.sigGen
        @(IDEATION hashAlgo MockDSIGN)
        ()
        Ideation.Env { Ideation.k = k
                     , Ideation.currentSlot = currentSlot
                     , Ideation.asips = asips
                     , Ideation.participants = participants
                     }
        Ideation.St { Ideation.subsips = subsips
                    , Ideation.wssips = wssips
                    , Ideation.wrsips = wrsips
                    , Ideation.ballots = ballots
                    , Ideation.sipdb = sipdb
                    }
    pure $! Ideation ideationPayload

  shrinkSignal (Ideation ideationPayload) =
    Ideation <$> STS.Gen.shrinkSignal @(IDEATION hashAlgo MockDSIGN) @() ideationPayload
  shrinkSignal (Implementation _) = error "Shrinking of IMPLEMENTATION signals is not defined yet."
