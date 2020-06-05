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

-- Required by CanApply
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# LANGUAGE MonoLocalBinds #-}

-- | Approval phase interface
--
module Cardano.Ledger.Update.Approval
  ( State
  , Error
    -- * State update functions
  , initialState
  , tick
  , apply
    -- ** Function constraints
  , CanApply
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

import qualified Ledger.Core as Core


import           Cardano.Ledger.Spec.Classes.HasAdversarialStakeRatio
                     (HasAdversarialStakeRatio)
import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     Signed, VKey, signatureVerifies)
import           Cardano.Ledger.Spec.Classes.HasStakeDistribution
                     (HasStakeDistribution,
                     TechnicalExperts (TechnicalExperts))
import           Cardano.Ledger.Spec.Classes.HasVotingPeriodsCap
                     (HasVotingPeriodsCap, maxVotingPeriods)
import           Cardano.Ledger.Spec.Classes.TracksSlotTime (TracksSlotTime,
                     currentSlot, stableAt)
import           Cardano.Ledger.Spec.State.ProposalsState (ProposalsState)
import qualified Cardano.Ledger.Spec.State.ProposalsState as Proposals
import           Cardano.Ledger.Spec.State.ProposalState (Decision)
import           Cardano.Ledger.Spec.State.ProposalState (Decision (Approved))
import           Cardano.Ledger.Spec.STS.Update.Approval.Data (ImplVote,
                     Implementation, ImplementationData, Submission)
import qualified Cardano.Ledger.Spec.STS.Update.Approval.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Data.Commit (Commit)
import qualified Cardano.Ledger.Spec.STS.Update.Data.Commit as Commit
import           Cardano.Ledger.Spec.STS.Update.Ideation.Data (SIPHash)
import qualified Cardano.Ledger.Update.Ideation as Ideation

data State p =
  State
  { submissionStableAt :: !(Map (Commit p (Implementation p)) Core.Slot)
    -- ^ Submitted commits, along with the timestamp (slot) at which they were
    -- submitted.
  , proposalsState   :: !(ProposalsState p (ImplementationData p))
    -- ^ Implementation-proposals state. See 'ProposalsState'.
  }

deriving instance Hashable p => Show (State p)

data Error p
  = ImplementationCommitAlreadySubmitted (Commit p (Implementation p))
  | CommitSignatureDoesNotVerify
  | NoCorrespondingCommit (Implementation p)
  | NoStableCommit (Implementation p)
  | ImplementationAlreadyRevealed (Implementation p)
  | NoApprovedSIP (SIPHash p)
  | VoteSignatureDoesNotVerify (Hash p (ImplementationData p))
  | VotePeriodHasNotStarted Core.Slot
                            (Hash p (ImplementationData p))
                            (ProposalsState p (ImplementationData p))
  | VotePeriodHasEnded Core.Slot
                       (Hash p (ImplementationData p))
                       (ProposalsState p (ImplementationData p))

deriving instance (Hashable p, HasSigningScheme p) => Eq (Error p)
deriving instance (Hashable p, HasSigningScheme p) => Show (Error p)

class HasApprovalError err p where
  getApprovalError :: err -> Maybe (Error p)

instance HasApprovalError (Error p) p where
  getApprovalError = Just . id

noImplementationCommit
  :: HasApprovalError err p => err -> Maybe (Implementation p)
noImplementationCommit err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    NoCorrespondingCommit impl -> Just impl
    _                          -> Nothing

noStableImplementationCommit
  :: HasApprovalError err p => err -> Maybe (Implementation p)
noStableImplementationCommit err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    NoStableCommit impl -> Just impl
    _                   -> Nothing

noApprovedSIP
  :: HasApprovalError err p => err -> Maybe (SIPHash p)
noApprovedSIP err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    NoApprovedSIP sipHash -> Just sipHash
    _                     -> Nothing

implementationVotePeriodHasNotStarted
  :: HasApprovalError err p => err -> Maybe (Hash p (ImplementationData p))
implementationVotePeriodHasNotStarted err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    VotePeriodHasNotStarted _ implementationHash _ -> Just implementationHash
    _                                              -> Nothing

implementationVotePeriodHasEnded
  :: HasApprovalError err p => err -> Maybe (Hash p (ImplementationData p))
implementationVotePeriodHasEnded err = do
  apprvErr <- getApprovalError err
  case apprvErr of
    VotePeriodHasEnded _ implementationHash _ -> Just implementationHash
    _                                         -> Nothing

--------------------------------------------------------------------------------
-- State update functions
--------------------------------------------------------------------------------

initialState :: Hashable p => State p
initialState =
  State
  { submissionStableAt = mempty
  , proposalsState     = mempty
  }

tick
  :: ( Hashable p
     , HasAdversarialStakeRatio env
     , HasStakeDistribution TechnicalExperts env p
     , TracksSlotTime env
     )
   => env -> State p -> State p
tick env st =
  st { proposalsState = Proposals.tally env TechnicalExperts (proposalsState st) }

apply
  :: ( CanApply env p
     )
  => env
  -> Ideation.State p
  -> Data.Payload p
  -> State p
  -> Either (Error p) (State p)
apply env _ideationSt (Data.Submit submission) st    = do
  let commit = Data.commit submission
  when (isImplementationSubmitted commit st)
    $ throwError (ImplementationCommitAlreadySubmitted commit)
  unless (signatureVerifies submission)
    $ throwError (CommitSignatureDoesNotVerify)
  pure $ st { submissionStableAt =
                Map.insert commit (stableAt env (currentSlot env)) (submissionStableAt st)
            }
apply env ideationSt (Data.Reveal implementation) st = do
  unless (isImplementationSubmitted commit st)
    $ throwError (NoCorrespondingCommit implementation)
  unless (isImplementationStablySubmitted env commit st)
    $ throwError (NoStableCommit implementation)
  when (isImplementationRevealed implementationHash st)
    $ throwError (ImplementationAlreadyRevealed implementation)
  unless (Ideation.isSIP implementedSIPHash Approved ideationSt)
    $ throwError (NoApprovedSIP implementedSIPHash)
  pure $ st { proposalsState =
                Proposals.reveal (currentSlot env)
                                 (maxVotingPeriods env)
                                 implementationData
                                 (proposalsState st)
            }

  where
    commit             = Commit.calcCommit implementation
    implementationHash = hash implementationData
    implementationData = Data.implPayload implementation
    implementedSIPHash = Data.implSIPHash implementation
apply env _ideationSt (Data.Vote vote) st            = do
  let implementationHash = Data.vImplHash vote
  unless (signatureVerifies vote)
    $ throwError (VoteSignatureDoesNotVerify implementationHash)
  unless (Proposals.votingPeriodHasStarted env implementationHash (proposalsState st))
    $ throwError (VotePeriodHasNotStarted (currentSlot env)
                                          implementationHash
                                          (proposalsState st)
                 )
  when (Proposals.votingPeriodHasEnded env implementationHash (proposalsState st))
    $ throwError (VotePeriodHasEnded (currentSlot env)
                                     implementationHash
                                     (proposalsState st)
                 )
  pure $ st { proposalsState =
              Proposals.updateBallot implementationHash vote (proposalsState st)
            }

class ( TracksSlotTime env
      , HasVotingPeriodsCap env

      , Hashable p
      , Signed p (Submission p)
      , Signed p (ImplVote p)

      , HasHash p (VKey p)
      , HasHash p (Implementation p)
      , HasHash p (Int, VKey p, Hash p (Implementation p))
      , HasHash p (ImplementationData p)
      ) => CanApply env p where

removeApproved
  :: State p
  -> ([(Hash p (ImplementationData p), (ImplementationData p))], State p)
removeApproved st =
  (approvedHashesAndProposals, st { proposalsState = proposalState'})
  where
    (approvedHashesAndProposals, proposalState') =
      Proposals.removeApproved (proposalsState st)

--------------------------------------------------------------------------------
-- State query functions
--------------------------------------------------------------------------------

class HasApprovalState st p where
  getApprovalState :: st -> State p

instance HasApprovalState (State p) p where
  getApprovalState = id

isImplementationSubmitted
  :: (Hashable p, HasApprovalState st p)
  => Commit p (Implementation p) -> st -> Bool
isImplementationSubmitted commit =
  isJust . Map.lookup commit . submissionStableAt . getApprovalState

isImplementationStablySubmitted
  :: ( Hashable p
     , TracksSlotTime env
     , HasApprovalState st p
     )
  => env -> Commit p (Implementation p) -> st -> Bool
isImplementationStablySubmitted env commit
  = maybe False (<= currentSlot env)
  . Map.lookup commit
  . submissionStableAt
  . getApprovalState

isImplementationRevealed
  :: (Hashable p, HasApprovalState st p)
  => Hash p (ImplementationData p) -> st -> Bool
isImplementationRevealed implHash =
  Proposals.isRevealed implHash . proposalsState . getApprovalState

isImplementationStablyRevealed
  :: ( Hashable p
     , TracksSlotTime env
     , HasApprovalState st p
     )
  => env -> Hash p (ImplementationData p) -> st -> Bool
isImplementationStablyRevealed env implHash =
  Proposals.isStablyRevealed env implHash . proposalsState . getApprovalState

isImplementation
  :: (Hashable p, HasApprovalState st p)
  => Hash p (ImplementationData p) -> Decision -> st -> Bool
isImplementation implHash d =
  Proposals.is implHash d . proposalsState . getApprovalState

isImplementationStably
  :: ( Hashable p
     , TracksSlotTime env
     , HasApprovalState st p
     )
  => env -> Hash p (ImplementationData p) -> Decision -> st -> Bool
isImplementationStably env implHash d =
  Proposals.isStably env implHash d . proposalsState . getApprovalState
