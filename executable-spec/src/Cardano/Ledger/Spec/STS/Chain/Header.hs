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


module Cardano.Ledger.Spec.STS.Chain.Header where

import           Data.Typeable (typeOf)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed, (?!))
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (BlockCount, Slot (Slot))

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.State.ActiveSIPs (ActiveSIPs)
import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs)
import           Cardano.Ledger.Spec.State.Ballot (Ballot)
import           Cardano.Ledger.Spec.State.RevealedSIPs (RevealedSIPs)
import           Cardano.Ledger.Spec.State.SIPsVoteResults (SIPsVoteResults)
import           Cardano.Ledger.Spec.State.StakeDistribution (StakeDistribution)
import           Cardano.Ledger.Spec.State.WhenRevealedSIPs (WhenRevealedSIPs)
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Update.Hupdate (HUPDATE)
import qualified Cardano.Ledger.Spec.STS.Update.Hupdate as Hupdate


-- | The Block HEADER STS
data HEADER p

data Env p
  = Env { k :: !BlockCount
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
        deriving (Eq, Show)

data St p
 = St { currentSlot :: !Slot
      , wrsips :: !(WhenRevealedSIPs p)
      , asips :: !(ActiveSIPs p)
      , vresips :: !(SIPsVoteResults p)
      , apprvsips :: !(ApprovedSIPs p)
      }
      deriving (Eq, Show, Generic)

data BHeader
  = BHeader { slot :: !Slot }
   deriving (Eq, Show, Generic)

deriving instance HasTypeReps BHeader

instance Sized BHeader where
  costsList bh = [(typeOf bh, 100)]

instance ( Hashable p
         ) => STS (HEADER p) where

  type Environment (HEADER p) = Env p

  type State (HEADER p) = St p

  type Signal (HEADER p) = BHeader

  data PredicateFailure (HEADER p)
    = BlockSlotNotIncreasing Slot Slot
    | HeaderFailure (PredicateFailure (HUPDATE p))
    deriving (Eq, Show)


  initialRules = [ ]

  transitionRules = [
    do
      TRC ( Env { k, sipdb, ballots, r_a
                , stakeDist, prvNoQuorum, prvNoMajority
                }
          , St  { currentSlot
                , wrsips
                , asips
                , vresips
                , apprvsips
                }
          , BHeader { slot }
          ) <- judgmentContext

      currentSlot < slot
        ?! BlockSlotNotIncreasing currentSlot slot
      Hupdate.St { Hupdate.wrsips = wrsips'
                 , Hupdate.asips = asips'
                 , Hupdate.vresips = vresips'
                 , Hupdate.apprvsips = apprvsips'
                 } <- trans @(HUPDATE p)
                      $ TRC ( Hupdate.Env { Hupdate.k = k
                                          , Hupdate.sipdb = sipdb
                                          , Hupdate.ballots = ballots
                                          , Hupdate.r_a = r_a
                                          , Hupdate.stakeDist = stakeDist
                                          , Hupdate.prvNoQuorum = prvNoQuorum
                                          , Hupdate.prvNoMajority = prvNoMajority
                                          }
                            , Hupdate.St { Hupdate.wrsips = wrsips
                                         , Hupdate.asips = asips
                                         , Hupdate.vresips = vresips
                                         , Hupdate.apprvsips = apprvsips
                                         }
                            , slot
                            )

      pure $ St { currentSlot = slot
                , wrsips = wrsips'
                , asips = asips'
                , vresips = vresips'
                , apprvsips = apprvsips'
                }
    ]

instance ( STS (HUPDATE p), STS (HEADER p)
         ) => Embed (HUPDATE p) (HEADER p)
    where
      wrapFailed = HeaderFailure

-- | Generate a valid next slot, given the current slot.
headerGen :: Slot -> QC.Gen BHeader
headerGen (Slot s) =
  BHeader . Slot . (s +) <$> QC.frequency [ (99, pure 1)
                                          , (1, pure 2)
                                          ]
