{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update where

import           Data.Bimap (Bimap)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           GHC.Generics (Generic)
import           Data.Typeable (Typeable)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition.Trace (traceSignals, TraceOrder (OldestFirst))
import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed)
import           Control.State.Transition.Generator (HasTrace, envGen, sigGen, genTrace)
import           Data.AbstractSize (HasTypeReps)
import           Ledger.Core (Slot (Slot), BlockCount)
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Cardano.Ledger.Spec.STS.Update.Ideation as Ideation
import qualified Cardano.Ledger.Spec.STS.Update.Implementation as Implementation
import           Cardano.Ledger.Spec.STS.Update.Ideation (IDEATION)
import           Cardano.Ledger.Spec.STS.Update.Implementation (IMPLEMENTATION)


data UPDATE hashAlgo


-- | As we incorporate more phases, like UP (or IMPLEMENTATION), we will be
-- adding more components to this environment.
--
-- See @Ideation.Env@ for more details on the meaning of each field.
data Env hashAlgo
  = Env
    { k :: !BlockCount
    , currentSlot :: !Slot
    , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
    , participants :: Bimap Core.VKey Core.SKey -- TODO: DISCUSS: I think we need to be consistent between using Core qualified and not.
    }
  deriving (Eq, Show, Generic)


data St hashAlgo
  = St
    { subsips :: !(Map (Data.Commit hashAlgo) (Data.SIP hashAlgo))
    , wssips :: !(Map (Data.Commit hashAlgo) Slot)
    , wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
    , ballots :: !(Map (Data.SIPHash hashAlgo) (Map Core.VKey Data.Confidence))
    , voteResultSIPs :: !(Map (Data.SIPHash hashAlgo) Data.VotingResult)
    , implementationSt :: State IMPLEMENTATION
    }
  deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup (St hashAlgo)
  deriving Monoid via GenericMonoid (St hashAlgo)


data UpdatePayload hashAlgo
  = Ideation (Data.IdeationPayload hashAlgo)
  | Implementation Data.ImplementationPayload
  deriving (Eq, Show, Generic)

deriving instance ( Typeable hashAlgo
                  , HasTypeReps hashAlgo
                  , HasTypeReps (Data.Commit hashAlgo)
                  , HashAlgorithm hashAlgo
                  , HasTypeReps (Hash hashAlgo Data.SIPData)
                  ) => HasTypeReps (UpdatePayload hashAlgo)

instance ( Typeable hashAlgo
         , HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo Data.SIPData)
         , HasTypeReps (Data.Commit hashAlgo)
         ) => Sized (UpdatePayload hashAlgo) where
  costsList _
    =  costsList (undefined :: (Data.IdeationPayload hashAlgo))
    ++ costsList (undefined :: Data.ImplementationPayload)

instance HashAlgorithm hashAlgo => STS (UPDATE hashAlgo) where

  type Environment (UPDATE hashAlgo) = (Env hashAlgo)

  type State (UPDATE hashAlgo) = (St hashAlgo)

  type Signal (UPDATE hashAlgo) = (UpdatePayload hashAlgo)

  data PredicateFailure (UPDATE hashAlgo)
    = IdeationsFailure (PredicateFailure (IDEATION hashAlgo))
    | ImplementationsFailure (PredicateFailure IMPLEMENTATION)
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( Env { k
                , currentSlot
                , asips
                , participants
                }
          , st@St { subsips
                  , wssips
                  , wrsips
                  , ballots
                  , voteResultSIPs
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
                        , Ideation.ballots = ballots'
                        , Ideation.voteResultSIPs = voteResultSIPs'
                        } <-
              trans @(IDEATION hashAlgo)
                $ TRC ( Ideation.Env { Ideation.k = k
                                     , Ideation.currentSlot = currentSlot
                                     , Ideation.asips = asips
                                     , Ideation.participants = participants
                                     }
                      , Ideation.St { Ideation.subsips = subsips
                                    , Ideation.wssips = wssips
                                    , Ideation.wrsips = wrsips
                                    , Ideation.ballots = ballots
                                    , Ideation.voteResultSIPs = voteResultSIPs
                                    }
                      , ideationPayload
                      )
            pure $ st { subsips = subsips'
                      , wssips = wssips'
                      , wrsips = wrsips'
                      , ballots = ballots'
                      , voteResultSIPs = voteResultSIPs'
                      }

        Implementation implementationPayload ->
          do
            implementationSt' <-
              trans @IMPLEMENTATION $
              TRC ( Implementation.Env currentSlot
                  , implementationSt
                  , implementationPayload
                  )
            pure $ st { implementationSt = implementationSt' }

    ]

instance HashAlgorithm hashAlgo => Embed (IDEATION hashAlgo) (UPDATE hashAlgo) where
  wrapFailed = IdeationsFailure

instance HashAlgorithm hashAlgo => Embed IMPLEMENTATION (UPDATE hashAlgo) where
  wrapFailed = ImplementationsFailure

data UPDATES hashAlgo

instance HashAlgorithm hashAlgo => STS (UPDATES hashAlgo) where

  type Environment (UPDATES hashAlgo) = Environment (UPDATE hashAlgo)

  type State (UPDATES hashAlgo) = State (UPDATE hashAlgo)

  type Signal (UPDATES hashAlgo) = [Signal (UPDATE hashAlgo)]

  data PredicateFailure (UPDATES hashAlgo)
    = UpdateFailure (PredicateFailure (UPDATE hashAlgo))
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC (env, st, updates) <- judgmentContext
      case updates of
        [] -> pure $! st
        (update:updates') ->
          do
            st' <- trans @(UPDATE hashAlgo) $ TRC (env, st, update)
            trans @(UPDATES hashAlgo) $ TRC (env, st', updates')
    ]


instance HashAlgorithm hashAlgo => Embed (UPDATE hashAlgo) (UPDATES hashAlgo) where
  wrapFailed = UpdateFailure

--------------------------------------------------------------------------------
-- Trace generators
--------------------------------------------------------------------------------

instance HashAlgorithm hashAlgo => HasTrace (UPDATES hashAlgo) where

  envGen traceLength = envGen @(UPDATE hashAlgo) traceLength

  sigGen env st
    =   traceSignals OldestFirst
    <$> genTrace @(UPDATE hashAlgo) 10 env st (sigGen @(UPDATE hashAlgo))
    -- TODO: we need to determine what is a realistic number of update
    -- transactions to be expected in a block.


instance HashAlgorithm hashAlgo => HasTrace (UPDATE hashAlgo) where

  envGen traceLength = do
    env <- envGen @(IDEATION hashAlgo) traceLength
    pure $! Env { k = Ideation.k env
                , currentSlot = Ideation.currentSlot env
                , asips = Ideation.asips env
                , participants = Ideation.participants env
                }

  sigGen  Env { k, currentSlot, asips, participants }
          St { subsips, wssips, wrsips, ballots, voteResultSIPs } =
    -- For now we generate ideation payload only.
    Ideation
      <$> sigGen @(IDEATION hashAlgo)
                  Ideation.Env { Ideation.k = k
                               , Ideation.currentSlot = currentSlot
                               , Ideation.asips = asips
                               , Ideation.participants = participants
                               }
                  Ideation.St { Ideation.subsips = subsips
                              , Ideation.wssips = wssips
                              , Ideation.wrsips = wrsips
                              , Ideation.ballots = ballots
                              , Ideation.voteResultSIPs = voteResultSIPs
                              }
