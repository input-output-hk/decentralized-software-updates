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

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set as Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Data.AbstractSize (HasTypeReps)
import           Data.Word (Word8)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext, trans, transitionRules, wrapFailed)
import           Cardano.Crypto.Hash (HashAlgorithm)

import           Ledger.Core (BlockCount, Slot, addSlot, dom, (*.), (-.), (⋪), (▷<=))
import qualified Ledger.Core as Core

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Tallysip (TALLYSIPS)
import qualified Cardano.Ledger.Spec.STS.Update.Tallysip as Tallysip

-- | The Header Update STS
-- Incorporates "update logic" processing
-- at the block header level
data HUPDATE hashAlgo

data Env hashAlgo
 = Env { k :: !BlockCount
         -- ^ Chain stability parameter.
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
  = St { wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
       , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
       , vresips :: !(Map (Data.SIPHash hashAlgo) Data.VotingResult)
       , apprvsips :: !(Set (Data.SIPHash hashAlgo))
         -- ^ Set of approved SIPs
       }
       deriving (Eq, Show, Generic)

instance ( HashAlgorithm hashAlgo
         , Data.AbstractSize.HasTypeReps hashAlgo
         ) =>  STS (HUPDATE hashAlgo) where

  type Environment (HUPDATE hashAlgo) = Env hashAlgo

  type State (HUPDATE hashAlgo) = St hashAlgo

  type Signal (HUPDATE hashAlgo) = Slot

  data PredicateFailure (HUPDATE hashAlgo)
    = ErrorOnHUpdate Slot Slot
    | HupdateFailure (PredicateFailure (TALLYSIPS hashAlgo))
    deriving (Eq, Show)


  initialRules = [
    do
      IRC Env { } <- judgmentContext
      pure $! St { wrsips = Map.empty
                 , asips = Map.empty
                 , vresips = Map.empty
                 , apprvsips = Set.empty
                 }
    ]

  transitionRules = [
    do
      TRC ( Env { k, sipdb, ballots, r_a, stakeDist
                , p_rvNoQuorum, p_rvNoMajority
                }
          , St  { wrsips
                , asips
                , vresips
                , apprvsips
                }
          , slot
          ) <- judgmentContext

      let
          -- Add newly revealed (but stable) SIPs to the active sips. Note that
          -- we place these new sips as arguments of the left hand side of the
          -- 'Map.union', since this operation is left biased.
          asips' = ( Map.mapWithKey  -- update asips slot with voting period end slot
                       (\sph _ -> slot `addSlot` (Data.votPeriodEnd sph sipdb))
                       (wrsips ▷<= (slot -. (2 *. k)))

                   )
                   `Map.union`
                   asips

          -- exclude old revealed SIPs
          wrsips' = dom asips' ⋪ wrsips

          -- Calculate SIPHashes to be tallied
          toTally = (map fst)
                      $ Map.toList
                      $ (asips' ▷<= (slot -. (2 *. k)))

          -- Prune asips, in order to avoid re-tallying of the same SIP
          asips'' = (Set.fromList toTally) ⋪ asips'
          --asips'' = Map.filterWithKey (\sh _ -> sh `notElem` toTally) asips'

      -- do the tallying and get the voting results (vresips)
      Tallysip.St { Tallysip.vresips = vresips'
                  , Tallysip.asips = asips'''
                  , Tallysip.apprvsips = apprvsips'
                  }
        <- trans @(TALLYSIPS hashAlgo)
              $ TRC ( Tallysip.Env { Tallysip.currentSlot = slot
                                   , Tallysip.sipdb = sipdb
                                   , Tallysip.ballots = ballots
                                   , Tallysip.r_a = r_a
                                   , Tallysip.stakeDist = stakeDist
                                   , Tallysip.p_rvNoQuorum = p_rvNoQuorum
                                   , Tallysip.p_rvNoMajority = p_rvNoMajority
                                   }
                    , Tallysip.St { Tallysip.vresips = vresips
                                  , Tallysip.asips = asips''
                                  , Tallysip.apprvsips = apprvsips
                                  }
                    , toTally
                    )

      pure $! St { wrsips = wrsips'
                 , asips = asips'''
                 , vresips = vresips'
                 , apprvsips = apprvsips'
                 }
    ]

instance ( HashAlgorithm hashAlgo
         , Data.AbstractSize.HasTypeReps hashAlgo
         ) => Embed (TALLYSIPS hashAlgo) (HUPDATE hashAlgo) where
  wrapFailed = HupdateFailure

