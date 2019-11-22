{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Tallysip where

import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext, trans, transitionRules,
                     wrapFailed, (?!))
import           Ledger.Core (Slot, addSlot, dom, (∈), (⨃))

import           Cardano.Ledger.Spec.Classes.Indexed ((!))
import           Cardano.Ledger.Spec.State.ActiveSIPs (ActiveSIPs)
import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs, registerApproval, isSIPApproved)
import           Cardano.Ledger.Spec.State.Ballot (Ballot, ballotFor, addVotes)
import           Cardano.Ledger.Spec.State.RevealedSIPs (RevealedSIPs, votingPeriodEnd)
import           Cardano.Ledger.Spec.State.SIPsVoteResults (SIPsVoteResults, getRevotingCounters)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution, totalStake)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Definitions (vThreshold)

-- | STS for tallying the votes of a single SIP
data TALLYSIP p

data Env p
 = Env { currentSlot :: !Slot
       , sipdb :: !(RevealedSIPs p)
       , ballots :: !(Ballot p)
       , r_a :: !Float
         -- ^ adversary stake ratio
       , stakeDist :: !(StakeDistribution p)
       , prvNoQuorum :: !Word8
         -- ^ How many times a revoting is allowed due to a no quorum result
       , prvNoMajority :: !Word8
         -- ^ How many times a revoting is allowed due to a no majority result
       }
       deriving (Show)

data St p
  = St { vresips :: !(SIPsVoteResults p)
         -- ^ Records the current voting result for each SIP
       , asips :: !(ActiveSIPs p)
      -- ^ Active SIP's. The slot in the range (of the map) determines when the
      -- voting period will end.
       , apprvsips :: !(ApprovedSIPs p)
         -- ^ Set of approved SIPs
       }
       deriving (Show, Generic)


instance (Hashable p) => STS (TALLYSIP p) where

  type Environment (TALLYSIP p) = (Env p)

  type State (TALLYSIP p) = (St p)

  type Signal (TALLYSIP p) = (Data.SIPHash p)

  data PredicateFailure (TALLYSIP p)
    =
      TallySIPFailure (Data.SIPHash p)
    | InvalidSIPHash (Data.SIPHash p)
    | SIPAlreadyApproved (Data.SIPHash p)

  initialRules = [
      do
        IRC Env { } <- judgmentContext
        pure $! St { vresips = mempty
                   , asips = mempty
                   , apprvsips = mempty
                   }
      ]

  transitionRules = [
    do
      TRC ( Env { currentSlot
                , sipdb
                , ballots
                , r_a
                , stakeDist
                , prvNoQuorum
                , prvNoMajority
                }
          , St  { vresips
                , asips
                , apprvsips
                }
          , sipHash
          ) <- judgmentContext

      sipHash ∈ dom sipdb ?! InvalidSIPHash sipHash

      not (isSIPApproved sipHash apprvsips) ?! SIPAlreadyApproved sipHash

      -- do the tally
      let
        (rvNoQ, rvNoM) = getRevotingCounters sipHash vresips

        -- count the votes for the specific SIP and store result
        vresips' =
          let ballotsOfSIP = ballotFor sipHash ballots
              vResult = addVotes stakeDist (Data.VotingResult 0 0 0 rvNoQ rvNoM) ballotsOfSIP
          in vresips ⨃ [(sipHash, vResult)]

        -- update state and in case of revoting, update both state and voting
        -- results
        (apprvsips', asips', vResult') =
          case tallyOutcome
                 (vresips'!sipHash)
                 stakeDist
                 prvNoQuorum
                 prvNoMajority
                 r_a of
            Data.Approved   ->
              ( registerApproval sipHash apprvsips
              , asips
              , (vresips'!sipHash)
              )
            Data.Rejected   ->
              ( apprvsips
              , asips
              , (vresips'!sipHash)
              )
            Data.NoQuorum   ->
              ( apprvsips
              , asips ⨃ [( sipHash
                         , currentSlot `addSlot` (votingPeriodEnd sipHash sipdb)
                         )
                        ]
              , Data.VotingResult 0 0 0 (rvNoQ + 1)  rvNoM
              )
            Data.NoMajority ->
              ( apprvsips
              , asips ⨃ [( sipHash,
                           currentSlot `addSlot` (votingPeriodEnd sipHash sipdb)
                         )
                        ]
              , Data.VotingResult 0 0 0 rvNoQ  (rvNoM + 1)
              )
            Data.Expired   ->
              ( apprvsips
              , asips
              , (vresips'!sipHash)
              )

        vresips'' = vresips' ⨃ [(sipHash, vResult')]
      pure $ St { vresips = vresips''
                , apprvsips = apprvsips'
                , asips = asips'
                }

    ]

-- | Return the outcome of the tally based on a  `VotingResult` and
-- a stake distribution.
tallyOutcome
  :: Data.VotingResult
  -> StakeDistribution p
  -> Word8  -- ^ max number of revoting for No Quorum
  -> Word8  -- ^ max number of revoting for No Majority
  -> Float  -- ^ adversary stake ratio
  -> Data.TallyOutcome
tallyOutcome vres stakeDistribution pNoQ pNoM r_a =
  if Data.stakePercentRound (Data.stakeInFavor vres) (totalStake stakeDistribution)
     > vThreshold r_a
    then
      Data.Approved
    else
      if Data.stakePercentRound (Data.stakeAgainst vres) (totalStake stakeDistribution)
         > vThreshold r_a
        then
          Data.Rejected
        else
          if Data.stakePercentRound (Data.stakeAbstain vres) (totalStake stakeDistribution)
             > vThreshold r_a
             && Data.rvNoQuorum vres < pNoQ
            then
              Data.NoQuorum
            else
              if Data.stakePercentRound (Data.stakeInFavor vres) (totalStake stakeDistribution)
                 <= vThreshold r_a
                 &&
                 Data.stakePercentRound (Data.stakeAgainst vres) (totalStake stakeDistribution)
                 <= vThreshold r_a
                 &&
                 Data.stakePercentRound (Data.stakeAbstain vres) (totalStake stakeDistribution)
                 <= vThreshold r_a
                 && Data.rvNoMajority vres < pNoM
                then
                  Data.NoMajority
                else
                  if Data.rvNoQuorum vres >= pNoQ || Data.rvNoMajority vres >= pNoM
                    then Data.Expired
                    else error "Tallysip.tallyOutcome: Reached a problemativ tally outcome."


deriving instance Eq (Data.SIPHash p) => Eq (PredicateFailure (TALLYSIP p))
deriving instance Show (Data.SIPHash p) => Show (PredicateFailure (TALLYSIP p))

-- | STS for tallying the votes of a
-- bunch of SIPs
data TALLYSIPS p

type ToTally p =  [Signal (TALLYSIP p)]

instance ( Hashable p
         ) => STS (TALLYSIPS p) where

  type Environment (TALLYSIPS p) = Environment (TALLYSIP p)

  type State (TALLYSIPS p) = State (TALLYSIP p)

  type Signal (TALLYSIPS p) = ToTally p

  data PredicateFailure (TALLYSIPS p)
    = TallySIPsFailure (PredicateFailure (TALLYSIP p))
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC ( env
          , st
          , toTally -- ToTally toTallyls
          ) <- judgmentContext
      case toTally of
        [] -> pure $! st
        (sh:siphashes) ->
          do
            st' <- trans @(TALLYSIP p) $ TRC (env, st, sh)
            trans @(TALLYSIPS p) $ TRC (env, st', siphashes)
    ]

instance ( STS (TALLYSIP p), STS (TALLYSIPS p)
         ) => Embed (TALLYSIP p) (TALLYSIPS p) where
    wrapFailed = TallySIPsFailure
