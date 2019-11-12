{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Tallysip where

import           Data.AbstractSize (HasTypeReps)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Set as Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           Cardano.Crypto.Hash (HashAlgorithm)
import           Cardano.Crypto.DSIGN.Class (VerKeyDSIGN)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext, trans, transitionRules,
                     wrapFailed, (?!))
import           Ledger.Core (Slot, addSlot, dom, (∈), (∉), (⨃))

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

-- | STS for tallying the votes of a single SIP
data TALLYSIP hashAlgo dsignAlgo

data Env hashAlgo dsignAlgo
 = Env { currentSlot :: !Slot
       , sipdb :: !(Map (Data.SIPHash hashAlgo) (Data.SIP hashAlgo dsignAlgo))
       , ballots :: !(Map (Data.SIPHash hashAlgo) (Map (VerKeyDSIGN dsignAlgo) Data.Confidence))
       , r_a :: !Float
         -- ^ adversary stake ratio
       , stakeDist :: !(Map (VerKeyDSIGN dsignAlgo) Data.Stake)
       , prvNoQuorum :: !Word8
         -- ^ How many times a revoting is allowed due to a no quorum result
       , prvNoMajority :: !Word8
         -- ^ How many times a revoting is allowed due to a no majority result
       }
   deriving (Generic)

deriving instance
  (Eq (VerKeyDSIGN dsignAlgo)) => Eq (Env hashAlgo dsignAlgo)

deriving instance
  (Show (VerKeyDSIGN dsignAlgo)) => Show (Env hashAlgo dsignAlgo)

data St hashAlgo dsignAlgo
  = St { vresips :: !(Map (Data.SIPHash hashAlgo) Data.VotingResult)
         -- ^ Records the current voting result for each SIP
       , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
      -- ^ Active SIP's. The slot in the range (of the map) determines when the
      -- voting period will end.
       , apprvsips :: !(Set (Data.SIPHash hashAlgo))
         -- ^ Set of approved SIPs
       }
       deriving (Generic)

deriving instance
  (Eq (VerKeyDSIGN dsignAlgo)) => Eq (St hashAlgo dsignAlgo)

deriving instance
  (Show (VerKeyDSIGN dsignAlgo)) => Show (St hashAlgo dsignAlgo)

instance
  ( Ord (VerKeyDSIGN dsignAlgo) -- TODO: remove this constraint
  ) => STS (TALLYSIP hashAlgo dsignAlgo) where

  type Environment (TALLYSIP hashAlgo dsignAlgo) = Env hashAlgo dsignAlgo

  type State (TALLYSIP hashAlgo dsignAlgo) = St hashAlgo dsignAlgo

  type Signal (TALLYSIP hashAlgo dsignAlgo) = Data.SIPHash hashAlgo

  data PredicateFailure (TALLYSIP hashAlgo dsignAlgo)
    =
      TallySIPFailure (Data.SIPHash hashAlgo)
    | InvalidSIPHash (Data.SIPHash hashAlgo)
    | SIPAlreadyApproved (Data.SIPHash hashAlgo)

    deriving (Eq, Show)

  initialRules = [
      do
        IRC Env { } <- judgmentContext
        pure $! St { vresips = Map.empty
                   , asips = Map.empty
                   , apprvsips = Set.empty
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

      sipHash ∉ apprvsips ?! SIPAlreadyApproved sipHash

      -- do the tally
      let
        -- get revoting counters for this SIP
        (rvNoQ, rvNoM) = case Map.lookup sipHash vresips of
                           Nothing -> (0,0)
                           Just vr -> ( Data.rvNoQuorum vr
                                      , Data.rvNoMajority vr
                                      )

        -- count the votes for the specific SIP and store result
        vresips' =
          let ballotsOfSIP = case Map.lookup sipHash ballots of
                               Nothing -> (Map.empty :: Map (VerKeyDSIGN dsignAlgo) Data.Confidence)
                               Just b  -> b

              vResult =
                if ballotsOfSIP == (Map.empty :: Map (VerKeyDSIGN dsignAlgo) Data.Confidence)
                  then
                    Data.VotingResult 0 0 0 rvNoQ rvNoM
                  else
                    Map.foldrWithKey'
                      (\vkey conf Data.VotingResult { Data.stakeInFavor
                                                    , Data.stakeAgainst
                                                    , Data.stakeAbstain
                                                    }
                          -> let stake = stakeDist!vkey
                             in case conf of
                                 Data.For -> Data.VotingResult
                                   (stakeInFavor + stake)
                                   stakeAgainst
                                   stakeAbstain
                                   rvNoQ
                                   rvNoM
                                 Data.Against -> Data.VotingResult
                                   stakeInFavor
                                   (stakeAgainst + stake)
                                   stakeAbstain
                                   rvNoQ
                                   rvNoM
                                 Data.Abstain -> Data.VotingResult
                                   stakeInFavor
                                   stakeAgainst
                                   (stakeAbstain + stake)
                                   rvNoQ
                                   rvNoM
                      )
                      (Data.VotingResult 0 0 0 rvNoQ rvNoM)
                      ballotsOfSIP
          in vresips ⨃ [(sipHash, vResult)]

        -- update state and
        -- in the case of revoting, update both state and voting result
        (apprvsips', asips', vResult') =
          case Data.tallyOutcome
                 (vresips'!sipHash)
                 stakeDist
                 prvNoQuorum
                 prvNoMajority
                 r_a of
            Data.Approved   ->
              ( Set.insert sipHash apprvsips
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
                         , currentSlot `addSlot` (Data.votPeriodEnd sipHash sipdb)
                         )
                        ]
              , Data.VotingResult 0 0 0 (rvNoQ + 1)  rvNoM
              )
            Data.NoMajority ->
              ( apprvsips
              , asips ⨃ [( sipHash,
                           currentSlot `addSlot` (Data.votPeriodEnd sipHash sipdb)
                         )
                        ]
              , Data.VotingResult 0 0 0 rvNoQ  (rvNoM + 1)
              )
            Data.Expired   ->
              ( apprvsips
              , asips
              , (vresips'!sipHash)
              )

        vresips'' = Map.insert sipHash vResult' vresips' -- this overwrites
                                                         -- existing pair
      pure $ St { vresips = vresips''
                , apprvsips = apprvsips'
                , asips = asips'
                }

    ]

-- | STS for tallying the votes of a
-- bunch of SIPs
data TALLYSIPS hashAlgo dsignAlgo

type ToTally hashAlgo dsignAlgo =  [Signal (TALLYSIP hashAlgo dsignAlgo)]

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , Ord (VerKeyDSIGN dsignAlgo) -- TODO: remove this constraint
         ) => STS (TALLYSIPS hashAlgo dsignAlgo) where

  type Environment (TALLYSIPS hashAlgo dsignAlgo) = Environment (TALLYSIP hashAlgo dsignAlgo)

  type State (TALLYSIPS hashAlgo dsignAlgo) = State (TALLYSIP hashAlgo dsignAlgo)

  type Signal (TALLYSIPS hashAlgo dsignAlgo) = ToTally hashAlgo dsignAlgo

  data PredicateFailure (TALLYSIPS hashAlgo dsignAlgo)
    = TallySIPsFailure (PredicateFailure (TALLYSIP hashAlgo dsignAlgo))
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
            st' <- trans @(TALLYSIP hashAlgo dsignAlgo) $ TRC (env, st, sh)
            trans @(TALLYSIPS hashAlgo dsignAlgo) $ TRC (env, st', siphashes)
    ]


instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         ,  Ord (VerKeyDSIGN dsignAlgo) -- TODO: remove this constraint
         ) => Embed (TALLYSIP hashAlgo dsignAlgo) (TALLYSIPS hashAlgo dsignAlgo) where
    wrapFailed = TallySIPsFailure
