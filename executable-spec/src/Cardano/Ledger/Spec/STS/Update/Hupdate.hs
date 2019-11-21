{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Cardano.Ledger.Spec.STS.Update.Hupdate where

import qualified Data.Set as Set
import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext, trans, transitionRules,
                     wrapFailed)

import           Ledger.Core (BlockCount, Slot, addSlot, dom, (*.), (-.), (⋪),
                     (▷<=), (⨃))

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.State.ActiveSIPs (ActiveSIPs)
import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs)
import           Cardano.Ledger.Spec.State.Ballot (Ballot)
import           Cardano.Ledger.Spec.State.RevealedSIPs (RevealedSIPs,
                     votingPeriodEnd)
import           Cardano.Ledger.Spec.State.SIPsVoteResults (SIPsVoteResults)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.State.WhenRevealedSIPs (WhenRevealedSIPs)
import           Cardano.Ledger.Spec.STS.Update.Tallysip (TALLYSIPS)
import qualified Cardano.Ledger.Spec.STS.Update.Tallysip as Tallysip

-- | The Header Update STS
-- Incorporates "update logic" processing
-- at the block header level
data HUPDATE p

data Env p
 = Env { k :: !BlockCount
         -- ^ Chain stability parameter.
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
  = St { wrsips :: !(WhenRevealedSIPs p)
       , asips :: !(ActiveSIPs p)
       , vresips :: !(SIPsVoteResults p)
       , apprvsips :: !(ApprovedSIPs p)
         -- ^ Set of approved SIPs
       }
       deriving (Show, Generic)

instance ( Hashable p
         ) =>  STS (HUPDATE p) where

  type Environment (HUPDATE p) = Env p

  type State (HUPDATE p) = St p

  type Signal (HUPDATE p) = Slot

  data PredicateFailure (HUPDATE p)
    = ErrorOnHUpdate Slot Slot
    | HupdateFailure (PredicateFailure (TALLYSIPS p))
    deriving (Eq, Show)


  initialRules = [
    do
      IRC Env { } <- judgmentContext
      pure $! St { wrsips = mempty
                 , asips = mempty
                 , vresips = mempty
                 , apprvsips = mempty
                 }
    ]

  transitionRules = [
    do
      TRC ( Env { k, sipdb, ballots, r_a, stakeDist
                , prvNoQuorum, prvNoMajority
                }
          , St  { wrsips
                , asips
                , vresips
                , apprvsips
                }
          , slot
          ) <- judgmentContext

      let
          -- Add newly revealed (but stable) SIPs to the active sips.
          asips' = asips
                 ⨃ [ (sipHash, votePeriodEnd)
                   | sipHash <- Set.toList $ dom (wrsips ▷<= (slot -. (2 *. k)))
                   , let votePeriodEnd = slot `addSlot` votingPeriodEnd sipHash sipdb
                   ]

          -- exclude old revealed SIPs
          wrsips' = dom asips' ⋪ wrsips

          -- Calculate SIPHashes to be tallied
          toTally = dom (asips' ▷<= (slot -. (2 *. k)))

          -- Prune asips, in order to avoid re-tallying of the same SIP
          asips'' = toTally ⋪ asips'

      -- do the tallying and get the voting results (vresips)
      Tallysip.St { Tallysip.vresips = vresips'
                  , Tallysip.asips = asips'''
                  , Tallysip.apprvsips = apprvsips'
                  }
        <- trans @(TALLYSIPS p)
              $ TRC ( Tallysip.Env { Tallysip.currentSlot = slot
                                   , Tallysip.sipdb = sipdb
                                   , Tallysip.ballots = ballots
                                   , Tallysip.r_a = r_a
                                   , Tallysip.stakeDist = stakeDist
                                   , Tallysip.prvNoQuorum = prvNoQuorum
                                   , Tallysip.prvNoMajority = prvNoMajority
                                   }
                    , Tallysip.St { Tallysip.vresips = vresips
                                  , Tallysip.asips = asips''
                                  , Tallysip.apprvsips = apprvsips
                                  }
                    , Set.toList toTally
                    )

      pure $! St { wrsips = wrsips'
                 , asips = asips'''
                 , vresips = vresips'
                 , apprvsips = apprvsips'
                 }
    ]

instance ( STS (TALLYSIPS p)
         , STS (HUPDATE p)
         ) => Embed (TALLYSIPS p) (HUPDATE p) where
  wrapFailed = HupdateFailure
