{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Spec.STS.Update.Tallysip where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Set as Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Data.AbstractSize (HasTypeReps)
import           Data.Word (Word8)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext,
                     trans, transitionRules, wrapFailed, (?!))
import           Ledger.Core (Slot, addSlot, (⨃), (∈), (∉), dom)
import qualified Ledger.Core as Core
import           Cardano.Crypto.Hash (HashAlgorithm)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Definitions (vThreshold)

-- | STS for tallying the votes of a single SIP
data TALLYSIP hashAlgo

data Env hashAlgo
 = Env { currentSlot :: !Slot
       , sipdb :: !(Map (Data.SIPHash hashAlgo) (Data.SIP hashAlgo))
       , ballots :: !(Map (Data.SIPHash hashAlgo) (Map Core.VKey Data.Confidence))
       , r_a :: !Float
         -- ^ adversary stake ratio
       , stakeDist :: !(Map Core.VKey Data.Stake)
       , p_rvNoQuorum :: !Word8
         -- ^ How many times a revoting is allowed due to a no quorum result
       , p_rvNoMajority :: !Word8
         -- ^ How many times a revoting is allowed due to a no majority result
       }
       deriving (Eq, Show)

data St hashAlgo
  = St { vresips :: !(Map (Data.SIPHash hashAlgo) Data.VotingResult)
         -- ^ Records the current voting result for each SIP
       , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
      -- ^ Active SIP's. The slot in the range (of the map) determines when the
      -- voting period will end.
       , apprvsips :: !(Set (Data.SIPHash hashAlgo))
         -- ^ Set of approved SIPs
       }
       deriving (Eq, Show, Generic)


instance STS (TALLYSIP hashAlgo) where

  type Environment (TALLYSIP hashAlgo) = (Env hashAlgo)

  type State (TALLYSIP hashAlgo) = (St hashAlgo)

  type Signal (TALLYSIP hashAlgo) = (Data.SIPHash hashAlgo)

  data PredicateFailure (TALLYSIP hashAlgo)
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
                , p_rvNoQuorum
                , p_rvNoMajority
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
                               Nothing -> (Map.empty :: Map Core.VKey Data.Confidence)
                               Just b  -> b

              vResult =
                if ballotsOfSIP == (Map.empty :: Map Core.VKey Data.Confidence)
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

        -- if approved then, update state of approved SIPs
        apprvsips' =
          if (fromIntegral $ Data.stakeInFavor (vresips'!sipHash))
             /
             (fromIntegral $ Data.totalStake stakeDist)
             * 100
             > (fromIntegral (vThreshold r_a :: Integer) :: Float)
            then
              Set.insert sipHash apprvsips
            else
              apprvsips

        -- if no-majority result, or no-quorum, calculate end of new voting period
        -- and update active sips state
        stakeInF = Data.stakeInFavor $ vresips'!sipHash
        stakeA = Data.stakeAgainst $ vresips'!sipHash
        stakeAb = Data.stakeAbstain $ vresips'!sipHash
        (asips', vResult') =
          if (  (fromIntegral $ Data.stakeAbstain (vresips'!sipHash))
                /
                (fromIntegral $ Data.totalStake stakeDist)
                * 100
                > (fromIntegral (vThreshold r_a :: Integer) :: Float)
             && rvNoQ <= p_rvNoQuorum
             )
            then
              (-- a revoting is due No Quorum - calc new voting period end
                asips ⨃ [(sipHash, currentSlot `addSlot` (Data.votPeriodEnd sipHash sipdb))]
              , Data.VotingResult stakeInF stakeA stakeAb (rvNoQ + 1)  rvNoM
              )
            else
              if ( (fromIntegral $ Data.stakeInFavor (vresips'!sipHash))
                   /
                   (fromIntegral $ Data.totalStake stakeDist)
                   * 100
                   <= (fromIntegral (vThreshold r_a :: Integer) :: Float)
                 &&
                   (fromIntegral $ Data.stakeAgainst (vresips'!sipHash))
                   /
                   (fromIntegral $ Data.totalStake stakeDist)
                   * 100
                   <= (fromIntegral (vThreshold r_a :: Integer) :: Float)
                 &&
                   (fromIntegral $ Data.stakeAbstain (vresips'!sipHash))
                   /
                   (fromIntegral $ Data.totalStake stakeDist)
                   * 100
                   <= (fromIntegral (vThreshold r_a :: Integer) :: Float)
                 && rvNoM <= p_rvNoMajority
                 )
                then
                  (-- a revoting is due No Majority - calc new voting period end
                    asips ⨃ [(sipHash, currentSlot `addSlot` (Data.votPeriodEnd sipHash sipdb))]
                  , Data.VotingResult stakeInF stakeA stakeAb rvNoQ  (rvNoM + 1)
                  )
                else
                  (asips, vresips'!sipHash)

        -- if a revoting took place then update rv counters
        vresips'' = Map.insert sipHash vResult' vresips' -- this overwrites
                                                         -- existing pair


      pure $ St { vresips = vresips''
                , apprvsips = apprvsips'
                , asips = asips'
                }

    ]

-- | STS for tallying the votes of a
-- bunch of SIPs
data TALLYSIPS hashAlgo

type ToTally hashAlgo =  [Signal (TALLYSIP hashAlgo)]

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         ) => STS (TALLYSIPS hashAlgo) where

  type Environment (TALLYSIPS hashAlgo) = Environment (TALLYSIP hashAlgo)

  type State (TALLYSIPS hashAlgo) = State (TALLYSIP hashAlgo)

  type Signal (TALLYSIPS hashAlgo) = ToTally hashAlgo

  data PredicateFailure (TALLYSIPS hashAlgo)
    =
     TallySIPsFailure (PredicateFailure (TALLYSIP hashAlgo))
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
            st' <- trans @(TALLYSIP hashAlgo) $ TRC (env, st, sh)
            trans @(TALLYSIPS hashAlgo) $ TRC (env, st', siphashes)
    ]


instance (HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         ) => Embed (TALLYSIP hashAlgo) (TALLYSIPS hashAlgo) where
    wrapFailed = TallySIPsFailure




