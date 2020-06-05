{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Ideation phase interface.
--
-- This module is intended to be used qualified.
--
module Cardano.Ledger.Update.Ideation
  ( State
  , Error
    -- * State update functions
  , initialState
  , tick
  , apply
    -- ** Function constraints
  , CanApply
    -- * State query functions
  , HasIdeationState (getIdeationState)
  , isSIPSubmitted
  , isSIPStablySubmitted
  , isSIPRevealed
  , isSIPStablyRevealed
  , isSIP
  , isSIPStably
  )
where

import           Control.Monad (unless, when)
import           Control.Monad.Except (throwError)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio)
import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     Signed, VKey, signatureVerifies)
import           Cardano.Ledger.Spec.Classes.HasStakeDistribution
                     (HasStakeDistribution, SIPExperts (SIPExperts))
import           Cardano.Ledger.Spec.Classes.HasVotingPeriodsCap
                     (HasVotingPeriodsCap, maxVotingPeriods)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime,
                     currentSlot, stableAt)
import           Cardano.Ledger.Spec.State.ProposalsState (ProposalsState)
import           Cardano.Ledger.Spec.State.ProposalState (Decision)
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit)

import qualified Cardano.Ledger.Spec.State.ProposalsState as Proposals
import qualified Cardano.Ledger.Spec.STS.Update.Data.Commit as Commit
import qualified Cardano.Ledger.Spec.STS.Update.Ideation.Data as Data
import qualified Ledger.Core as Core


-- | Ideation state parameterized over the hashing and cryptographic functions.
data State p =
  State
  { submissionStableAt :: !(Map (Commit p (Data.SIP p)) Core.Slot)
  , proposalsState     :: !(ProposalsState p Data.SIPData)
  }

deriving instance Hashable p => Show (State p)

data Error p
  = SIPCommitAlreadySubmitted (Data.SIPCommit p)
  | CommitSignatureDoesNotVerify (Data.SIPCommit p)
  | NoCorrespondingCommit (Data.SIP p)
  | NoStableCommit (Data.SIP p)
  | AlreadyRevealed (Data.SIP p)
  | VoteSignatureDoesNotVerify (Data.VoteForSIP p)
  | VotePeriodHasNotStarted Core.Slot (Data.VoteForSIP p) (ProposalsState p Data.SIPData)
  | VotePeriodHasEnded Core.Slot (Data.VoteForSIP p) (ProposalsState p Data.SIPData)

deriving instance (Hashable p, HasSigningScheme p) => Eq (Error p)
deriving instance (Hashable p, HasSigningScheme p) => Show (Error p)

--------------------------------------------------------------------------------
-- State update functions
--------------------------------------------------------------------------------

initialState :: Hashable p => State p
initialState =
  State
  { submissionStableAt = mempty
  , proposalsState     = mempty
  }

-- | Tick to a target slot. The environment contains the slot we're ticking to.
--
-- TODO: this function relies on the fact that we tick at each slot. We could
-- check this by adding the last applied slot to the state, but this would
-- duplicate state and checks. It is not clear right not if this is a compromise
-- worth doing.
tick
  :: ( Hashable p
     , TracksSlotTime env
     , HasAdversarialStakeRatio env
     , HasStakeDistribution SIPExperts env p
     )
  => env -> State p -> State p
tick env st =
  st { proposalsState = Proposals.tally env SIPExperts (proposalsState st) }

apply
  :: ( CanApply env p
     )
  => env -> Data.Payload p -> State p -> Either (Error p) (State p)
apply env (Data.Submit sipc _) st = do
  when (isSIPSubmitted commit st)
    $ throwError (SIPCommitAlreadySubmitted sipc)
  unless (signatureVerifies sipc)
    $ throwError (CommitSignatureDoesNotVerify sipc)
  pure $ st { submissionStableAt =
              Map.insert commit (stableAt env (currentSlot env)) (submissionStableAt st)
            }
  where
    commit = Data.commit sipc
apply env (Data.Reveal sip) st    = do
  unless (isSIPSubmitted commit st)
    $ throwError (NoCorrespondingCommit sip)
  unless (isSIPStablySubmitted env commit st)
    $ throwError (NoStableCommit sip)
  when (isSIPRevealed sipDataHash st)
    $ throwError (AlreadyRevealed sip)
  pure $ st { proposalsState =
                Proposals.reveal (currentSlot env) (maxVotingPeriods env) sipData (proposalsState st)
            }
  where
    commit      = Commit.calcCommit sip
    sipData     = Data.sipPayload sip
    sipDataHash = Data.sipHash sip
apply env (Data.Vote vote) st     = do
  unless (signatureVerifies vote)
    $ throwError (VoteSignatureDoesNotVerify vote)
  let
    sipHash = Data.votedsipHash vote
  unless (Proposals.votingPeriodHasStarted env sipHash (proposalsState st))
    $ throwError (VotePeriodHasNotStarted (currentSlot env) vote (proposalsState st))
  when (Proposals.votingPeriodHasEnded env sipHash (proposalsState st))
    $ throwError (VotePeriodHasEnded (currentSlot env) vote (proposalsState st))
  pure $ st { proposalsState =
              Proposals.updateBallot sipHash vote (proposalsState st)
            }


-- | Constraints required for applying the ideation payload.
class (TracksSlotTime env
     , HasVotingPeriodsCap env
     , Hashable p
     , HasHash p (Data.SIP p)
     , HasHash p Data.SIPData
     , HasHash p (Int, VKey p, Hash p (Data.SIP p))
     , HasHash p (VKey p) -- needed for 'updateBallot'

     , Signed p (Data.SIPCommit p)
     , Signed p (Data.VoteForSIP p)
     ) => CanApply env p where

--------------------------------------------------------------------------------
-- State query functions
--------------------------------------------------------------------------------

class HasIdeationState st p where
  getIdeationState :: st -> State p

instance HasIdeationState (State p) p where
  getIdeationState = id

isSIPSubmitted
  :: (Hashable p, HasIdeationState st p)
  => Commit p (Data.SIP p)
  -> st
  -> Bool
isSIPSubmitted sipCommit =
  Map.member sipCommit . submissionStableAt . getIdeationState

isSIPStablySubmitted
  :: ( Hashable p
     , TracksSlotTime env
     , HasIdeationState st p
     )
  => env
  -> Commit p (Data.SIP p)
  -> st
  -> Bool
isSIPStablySubmitted env sipCommit
  = maybe False (<= currentSlot env)
  . Map.lookup sipCommit
  . submissionStableAt
  . getIdeationState

isSIPRevealed
  :: ( Hashable p
     , HasIdeationState st p
     )
  => Hash p Data.SIPData -> st -> Bool
isSIPRevealed sipDataHash =
  Proposals.isRevealed sipDataHash . proposalsState . getIdeationState

isSIPStablyRevealed
  :: ( Hashable p
     , TracksSlotTime env
     , HasIdeationState st p
     )
  => env -> Hash p Data.SIPData -> st -> Bool
isSIPStablyRevealed env sipDataHash =
  Proposals.isStablyRevealed env sipDataHash . proposalsState . getIdeationState

isSIP
  :: ( Hashable p
     , HasIdeationState st p
     )
  => Hash p Data.SIPData -> Decision -> st -> Bool
isSIP sipDataHash d =
  Proposals.is sipDataHash d . proposalsState . getIdeationState

isSIPStably
  :: ( Hashable p
     , TracksSlotTime env
     , HasIdeationState st p
     )
  => env -> Hash p Data.SIPData -> Decision -> st -> Bool
isSIPStably env sipDataHash d =
  Proposals.isStably env sipDataHash d . proposalsState . getIdeationState
