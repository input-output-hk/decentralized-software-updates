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

import           Data.Map.Strict (Map)
import           Data.Set as Set (Set)
import           Data.Typeable (typeOf)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import qualified Test.QuickCheck as QC

import           Cardano.Crypto.DSIGN.Class (VerKeyDSIGN)
import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed, (?!))
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (BlockCount, Slot (Slot))

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Update.Data (SIPData)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Hupdate (HUPDATE)
import qualified Cardano.Ledger.Spec.STS.Update.Hupdate as Hupdate


-- | The Block HEADER STS
data HEADER hashAlgo dsignAlgo

data Env hashAlgo dsignAlgo
  = Env { k :: !BlockCount
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

deriving instance
  (Eq (VerKeyDSIGN dsignAlgo)) => Eq (Env hashAlgo dsignAlgo)

deriving instance
  (Show (VerKeyDSIGN dsignAlgo)) => Show (Env hashAlgo dsignAlgo)

data St hashAlgo
 = St { currentSlot :: !Slot
      , wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
      , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
      , vresips :: !(Map (Data.SIPHash hashAlgo) Data.VotingResult)
      , apprvsips :: !(Set (Data.SIPHash hashAlgo))
      }
      deriving (Eq, Show, Generic)

data BHeader
  = BHeader { slot :: !Slot }
   deriving (Eq, Show, Generic)


deriving instance HasTypeReps BHeader

instance Sized BHeader where
  costsList bh = [(typeOf bh, 100)]

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , Ord (VerKeyDSIGN dsignAlgo) -- TODO: remove this constraint
         ) => STS (HEADER hashAlgo dsignAlgo) where

  type Environment (HEADER hashAlgo dsignAlgo) = Env hashAlgo dsignAlgo

  type State (HEADER hashAlgo dsignAlgo) = St hashAlgo

  type Signal (HEADER hashAlgo dsignAlgo) = BHeader

  data PredicateFailure (HEADER hashAlgo dsignAlgo)
    = BlockSlotNotIncreasing Slot Slot
    | HeaderFailure (PredicateFailure (HUPDATE hashAlgo dsignAlgo))
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
                 } <- trans @(HUPDATE hashAlgo dsignAlgo)
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

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , Ord (VerKeyDSIGN dsignAlgo) -- TODO: remove this constraint
         )
  => Embed (HUPDATE hashAlgo dsignAlgo) (HEADER hashAlgo dsignAlgo)
    where
      wrapFailed = HeaderFailure

-- | Generate a valid next slot, given the current slot.
headerGen :: Slot -> QC.Gen BHeader
headerGen (Slot s) =
  BHeader . Slot . (s +) <$> QC.frequency [ (99, pure 1)
                                          , (1, pure 2)
                                          ]
