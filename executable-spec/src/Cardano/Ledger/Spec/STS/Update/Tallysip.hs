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

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext, trans, transitionRules, wrapFailed)
import           Ledger.Core (Slot, addSlot, (⨃))
import qualified Ledger.Core as Core
import           Cardano.Crypto.Hash (HashAlgorithm)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

-- | STS for tallying the votes of a single SIP
data TALLYSIP hashAlgo

data Env hashAlgo
 = Env { currentSlot :: !Slot
       , sipdb :: !(Map (Data.SIPHash hashAlgo) (Data.SIP hashAlgo))
       , ballots :: !(Map (Data.SIPHash hashAlgo) (Map Core.VKey Data.Confidence))
       , vThreshold :: !Data.VThreshold
       , stakeDist :: !(Map Core.VKey Data.Stake)
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
                , vThreshold
                , stakeDist
                }
          , St  { vresips
                , asips
                , apprvsips
                }
          , sipHash
          ) <- judgmentContext

      -- do the tally
      let
        -- count the votes for the specific SIP and store result
        vresips' =
          let ballotsOfSIP = case Map.lookup sipHash ballots of
                               Nothing -> (Map.empty :: Map Core.VKey Data.Confidence)
                               Just b  -> b

              vResult =
                if ballotsOfSIP == (Map.empty :: Map Core.VKey Data.Confidence)
                  then
                    Data.VotingResult 0 0 0
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
                                 Data.Against -> Data.VotingResult
                                   stakeInFavor
                                   (stakeAgainst + stake)
                                   stakeAbstain
                                 Data.Abstain -> Data.VotingResult
                                   stakeInFavor
                                   stakeAgainst
                                   (stakeAbstain + stake)
                      )
                      (Data.VotingResult 0 0 0)
                      ballotsOfSIP
          in vresips ⨃ [(sipHash, vResult)]

        -- if approved then, update state of approved SIPs
        apprvsips' =
          if Data.stakeInFavor (vresips'!sipHash) > fromIntegral vThreshold
            then
              Set.insert sipHash apprvsips
            else
              apprvsips

        -- if no-majority result, or no-quorum, calculate end of new voting period
        -- and update active sips state
        asips' =
          if Data.stakeAbstain (vresips'!sipHash) > fromIntegral vThreshold
             ||
             (  Data.stakeInFavor (vresips'!sipHash) <= fromIntegral vThreshold
             && Data.stakeAgainst (vresips'!sipHash) <= fromIntegral vThreshold
             && Data.stakeAbstain (vresips'!sipHash) <= fromIntegral vThreshold
             )
            then -- a revoting is due - calc new voting period end
              asips ⨃ [(sipHash, currentSlot `addSlot` (votPeriodEnd sipHash))]
            else
              asips
        votPeriodEnd siphash =  Data.vpDurationToSlotCnt
                                $ Data.votPeriodDuration
                                . Data.metadata
                                . Data.sipPayload
                                $ (sipdb!siphash)

      pure $ St { vresips = vresips'
                , apprvsips = apprvsips'
                , asips = asips'
                }

    ]

-- | STS for tallying the votes of a
-- bunch of SIPs
data TALLYSIPS hashAlgo

-- | Candidate SIPs for tallying
-- data ToTally hashAlgo = ToTally ![(Data.SIPHash hashAlgo)]  -- ![Signal (TALLYSIP hashAlgo)]
--   deriving (Eq, Show)

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




