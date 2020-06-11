{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

-- | Approval phase interface
--
module Cardano.Ledger.Update.Approval
  ( State
  , Error
    -- * State update functions
  , initialState
  , tick
  , apply
    -- * State query functions
  , HasApprovalState (getApprovalState)
  , removeApproved
  , isImplementation
  , isImplementationStably
  , isImplementationSubmitted
  , isImplementationStablySubmitted
  , isImplementationRevealed
  , isImplementationStablyRevealed
  -- * Error query
  , HasApprovalError (getApprovalError)
  , noApprovedSIP
  , noImplementationCommit
  , noStableImplementationCommit
  , implementationVotePeriodHasNotStarted
  , implementationVotePeriodHasEnded
  )
where

import           Control.Monad (unless, when)
import           Control.Monad.Except (throwError)
import           Data.Map.Strict (Map)
import           Data.Maybe (isJust)

import qualified Data.Map.Strict as Map

import Cardano.Slotting.Slot (SlotNo)

import           Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio)
import           Cardano.Ledger.Update.Env.HasStakeDistribution
                     (HasStakeDistribution)
import           Cardano.Ledger.Update.Env.HasVotingPeriodsCap
                     (HasVotingPeriodsCap, maxVotingPeriods)
import           Cardano.Ledger.Update.Env.TracksSlotTime (TracksSlotTime,
                     currentSlot, stableAt)

import qualified Cardano.Ledger.Update.Ideation as Ideation

import           Cardano.Ledger.Update.ProposalsState (ProposalsState)
import qualified Cardano.Ledger.Update.ProposalsState as Proposals
import           Cardano.Ledger.Update.ProposalState (Decision)
import           Cardano.Ledger.Update.ProposalState (Decision (Approved))

import           Cardano.Ledger.Update.Proposal hiding (commit)
import qualified Cardano.Ledger.Update.Proposal as Proposal

data State impl =
  State
  { submissionStableAt :: !(Map (Commit (Revelation impl)) SlotNo)
    -- ^ Submitted commits, along with the timestamp (slot) at which they were
    -- submitted.
  , proposalsState   :: !(ProposalsState impl)
    -- ^ Implementation-proposals state. See 'ProposalsState'.
  }

deriving instance Proposal impl => Show (State impl)

data Error sip impl
  = ImplementationCommitAlreadySubmitted (Commit (Revelation impl))
  | CommitSignatureDoesNotVerify (Commit (Revelation impl))
  | NoCorrespondingCommit (Revelation impl)
  | NoStableCommit (Revelation impl)
  | ImplementationAlreadyRevealed (Revelation impl)
  | NoApprovedSIP (Id sip)
  | VoteSignatureDoesNotVerify (Vote impl)
  | VotePeriodHasNotStarted SlotNo
                            (Id impl)
                            (ProposalsState impl)
  | VotePeriodHasEnded SlotNo
                       (Id impl)
                       (ProposalsState impl)

deriving instance Implementation sip impl
                  => Show (Error sip impl)

class HasApprovalError err sip impl
     | err -> sip, err -> impl      where
  getApprovalError :: err -> Maybe (Error sip impl)

instance HasApprovalError (Error sip impl) sip impl where
  getApprovalError = Just . id

noImplementationCommit
  :: HasApprovalError err sip impl => err -> Maybe (Revelation impl)
noImplementationCommit err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    NoCorrespondingCommit impl -> Just impl
    _                          -> Nothing

noStableImplementationCommit
  :: HasApprovalError err sip impl => err -> Maybe (Revelation impl)
noStableImplementationCommit err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    NoStableCommit impl -> Just impl
    _                   -> Nothing

noApprovedSIP
  :: HasApprovalError err sip impl => err -> Maybe (Id sip)
noApprovedSIP err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    NoApprovedSIP sipHash -> Just sipHash
    _                     -> Nothing

implementationVotePeriodHasNotStarted
  :: HasApprovalError err sip impl => err -> Maybe (Id impl)
implementationVotePeriodHasNotStarted err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    VotePeriodHasNotStarted _ implementationHash _ -> Just implementationHash
    _                                              -> Nothing

implementationVotePeriodHasEnded
  :: HasApprovalError err sip impl => err -> Maybe (Id impl)
implementationVotePeriodHasEnded err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    VotePeriodHasEnded _ implementationHash _ -> Just implementationHash
    _                                         -> Nothing

--------------------------------------------------------------------------------
-- State update functions
--------------------------------------------------------------------------------

initialState :: Proposal impl => State impl
initialState =
  State
  { submissionStableAt = mempty
  , proposalsState     = Proposals.initialState
  }

tick
  :: ( HasAdversarialStakeRatio env
     , HasStakeDistribution  env (Id (Voter impl))
     , TracksSlotTime env
     , Proposal impl
     )
   => env -> State impl -> State impl
tick env st =
  st { proposalsState = Proposals.tally env (proposalsState st) }

apply
  :: ( HasVotingPeriodsCap env
     , TracksSlotTime env

     , Proposal sip
     , Implementation sip impl
     )
  => env
  -> Ideation.State sip
  -> Payload impl
  -> State impl
  -> Either (Error sip impl) (State impl)
apply env _ideationSt (Submit submission) st = do
  let commit = revelationCommit submission
  when (isImplementationSubmitted commit st)
    $ throwError (ImplementationCommitAlreadySubmitted commit)
  unless (signatureVerifies submission)
    $ throwError (CommitSignatureDoesNotVerify commit)
  pure $ st { submissionStableAt =
                Map.insert commit (stableAt env (currentSlot env)) (submissionStableAt st)
            }
apply env ideationSt (Reveal revelation) st  = do
  let commit = Proposal.commit revelation
  unless (isImplementationSubmitted commit st)
    $ throwError (NoCorrespondingCommit revelation)
  unless (isImplementationStablySubmitted env commit st)
    $ throwError (NoStableCommit revelation)
  let implementationId = _id (proposal revelation)
      implementedSIPId = preProposalId (proposal revelation)
  when (isImplementationRevealed implementationId st)
    $ throwError (ImplementationAlreadyRevealed revelation)
  unless (Ideation.isSIP implementedSIPId Approved ideationSt)
    $ throwError (NoApprovedSIP implementedSIPId)
  pure $ st { proposalsState =
                Proposals.reveal (currentSlot env)
                                 (maxVotingPeriods env)
                                 (proposal revelation)
                                 (proposalsState st)
            }
apply env _ideationSt (Cast vote) st         = do
  unless (signatureVerifies vote)
    $ throwError (VoteSignatureDoesNotVerify vote)
  let implementationId = candidate vote
  unless (Proposals.votingPeriodHasStarted env implementationId (proposalsState st))
    $ throwError (VotePeriodHasNotStarted (currentSlot env)
                                          implementationId
                                          (proposalsState st)
                 )
  when (Proposals.votingPeriodHasEnded env implementationId (proposalsState st))
    $ throwError (VotePeriodHasEnded (currentSlot env)
                                     implementationId
                                     (proposalsState st)
                 )
  pure $ st { proposalsState =
              Proposals.updateBallot implementationId vote (proposalsState st)
            }

removeApproved
  :: State impl
  -> ([impl], State impl)
removeApproved st =
  (approvedIdsAndProposals, st { proposalsState = proposalState'})
  where
    (approvedIdsAndProposals, proposalState') =
      Proposals.removeApproved (proposalsState st)

--------------------------------------------------------------------------------
-- State query functions
--------------------------------------------------------------------------------

class HasApprovalState st impl | st -> impl where
  getApprovalState :: st -> State impl

instance HasApprovalState (State impl) impl where
  getApprovalState = id

isImplementationSubmitted
  :: (Proposal impl, HasApprovalState st impl)
  => Commit (Revelation impl) -> st -> Bool
isImplementationSubmitted commit =
  isJust . Map.lookup commit . submissionStableAt . getApprovalState

isImplementationStablySubmitted
  :: ( Proposal impl
     , TracksSlotTime env
     , HasApprovalState st impl
     )
  => env -> Commit (Revelation impl) -> st -> Bool
isImplementationStablySubmitted env commit
  = maybe False (<= currentSlot env)
  . Map.lookup commit
  . submissionStableAt
  . getApprovalState

isImplementationRevealed
  :: (Proposal impl, HasApprovalState st impl)
  => Id impl -> st -> Bool
isImplementationRevealed implementationId =
  Proposals.isRevealed implementationId . proposalsState . getApprovalState

isImplementationStablyRevealed
  :: ( Proposal impl
     , TracksSlotTime env
     , HasApprovalState st impl
     )
  => env -> Id impl -> st -> Bool
isImplementationStablyRevealed env implementationId =
  Proposals.isStablyRevealed env implementationId . proposalsState . getApprovalState

isImplementation
  :: (Proposal impl, HasApprovalState st impl)
  => Id impl -> Decision -> st -> Bool
isImplementation implementationId d =
  Proposals.is implementationId d . proposalsState . getApprovalState

isImplementationStably
  :: ( Proposal impl
     , TracksSlotTime env
     , HasApprovalState st impl
     )
  => env -> Id impl -> Decision -> st -> Bool
isImplementationStably env implementationId d =
  Proposals.isStably env implementationId d . proposalsState . getApprovalState
