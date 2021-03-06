{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Definition of the system under test interface
module Test.Cardano.Ledger.Update.Properties.UpdateSUT
  ( UpdateSUT
  , SUTAct (TickAct, UpdateAct)
  , SUTSt (UpdateSt, unUpdateSt)
  -- * Extraction of update data from SUT actions
  -- ** Ideation
  , getSubmittedSIP
  , getRevealedSIP
  , getVotedSIP
  , getSIPVoter
  , getSIPVote
  , getSIPVoteOf
  -- ** Approval
  , getSubmittedImpl
  , getRevealedImpl
  , getVotedImpl
  , getImplVote
  , getImplVoteOf
  -- ** Activation
  , getEndorsement
  , getEndorsedVersion
  )
where

import           Cardano.Ledger.Update.Env.TracksSlotTime (currentSlot)
import           Cardano.Ledger.Update.Proposal

import qualified Cardano.Ledger.Update as Update
import qualified Cardano.Ledger.Update.Proposal as Proposal

import           SystemUnderTest

import           Test.Cardano.Ledger.Update.Data
import           Test.Cardano.Ledger.Update.Interface

data UpdateSUT

instance SUT UpdateSUT where

  newtype SUTSt UpdateSUT = UpdateSt { unUpdateSt :: IState }
    deriving (Show, Eq)

  data SUTAct UpdateSUT
    = TickAct
    -- ^ Tick for one slot. We do not make the number of slots variable since
    -- this complicates writing trace properties of the update mechanism.
    | UpdateAct (Update.Payload MockSIP MockImpl)
    -- TODO: we need actions to update the environment: in particular an action
    -- for changing the stake.
    deriving (Show)

  apply sutAct (UpdateSt st)
    = either (const Nothing) (Just . UpdateSt)
    $ case sutAct of
        TickAct           -> slotTick (currentSlot st + 1) st
        UpdateAct payload -> applyUpdate payload st
        -- TODO: here we need a case for applying a stake change.

instance Update.HasIdeationState (SUTSt UpdateSUT) MockSIP where
  getIdeationState (UpdateSt st) = Update.getIdeationState st

instance Update.HasApprovalState (SUTSt UpdateSUT) MockImpl where
  getApprovalState (UpdateSt st) = Update.getApprovalState st

instance Update.HasActivationState (SUTSt UpdateSUT) MockSIP MockImpl where
  getActivationState (UpdateSt st) = Update.getActivationState st

getSubmittedSIP :: SUTAct UpdateSUT -> Maybe (Submission MockSIP)
getSubmittedSIP (UpdateAct (Update.Ideation (Proposal.Submit s))) = Just s
getSubmittedSIP _                                                 = Nothing

getRevealedSIP :: SUTAct UpdateSUT -> Maybe (Revelation MockSIP)
getRevealedSIP (UpdateAct (Update.Ideation (Proposal.Reveal r))) = Just r
getRevealedSIP _                                                 = Nothing

getSIPVote :: SUTAct UpdateSUT -> Maybe (Vote MockSIP)
getSIPVote (UpdateAct (Update.Ideation (Proposal.Cast vote))) = Just vote
getSIPVote _                                                  = Nothing

getVotedSIP :: SUTAct UpdateSUT -> Maybe (Id MockSIP)
getVotedSIP = fmap candidate . getSIPVote

getSIPVoter
  :: Confidence
  -> Id MockSIP
  -> SUTAct UpdateSUT
  -> Maybe (Id (Voter MockSIP))
getSIPVoter conf sipId act = do
  vote <- getSIPVoteOf sipId act
  if confidence vote == conf
    then pure $! voter vote
    else Nothing

getSIPVoteOf
  :: Id MockSIP
  -> SUTAct UpdateSUT
  -> Maybe (Vote MockSIP)
getSIPVoteOf sipId act =
  case getSIPVote act of
    Just vote
      | candidate vote == sipId -> Just vote
    _ -> Nothing

getSubmittedImpl :: SUTAct UpdateSUT -> Maybe (Submission MockImpl)
getSubmittedImpl (UpdateAct (Update.Approval (Proposal.Submit s))) = Just s
getSubmittedImpl _                                                 = Nothing

getRevealedImpl ::  SUTAct UpdateSUT -> Maybe (Revelation MockImpl)
getRevealedImpl (UpdateAct (Update.Approval (Proposal.Reveal r))) = Just r
getRevealedImpl _                                                 = Nothing

getImplVote :: SUTAct UpdateSUT -> Maybe (Vote MockImpl)
getImplVote (UpdateAct (Update.Approval (Proposal.Cast vote))) = Just vote
getImplVote _                                                  = Nothing

getVotedImpl :: SUTAct UpdateSUT -> Maybe (Id MockImpl)
getVotedImpl = fmap candidate . getImplVote

getImplVoteOf
  :: Id MockImpl
  -> SUTAct UpdateSUT
  -> Maybe (Vote MockImpl)
getImplVoteOf implId act =
  case getImplVote act of
    Just vote
      | candidate vote == implId -> Just vote
    _ -> Nothing

getEndorsement :: SUTAct UpdateSUT -> Maybe (Update.Endorsement MockSIP MockImpl)
getEndorsement (UpdateAct (Update.Activation e)) = Just e
getEndorsement _                                 = Nothing

getEndorsedVersion :: SUTAct UpdateSUT -> Maybe (Version (Protocol MockImpl))
getEndorsedVersion = fmap Update.endorsedVersion . getEndorsement
