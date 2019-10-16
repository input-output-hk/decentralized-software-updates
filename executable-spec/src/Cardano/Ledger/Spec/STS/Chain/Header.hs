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
import qualified Data.Map.Strict as Map
import           Data.Typeable (typeOf)
import           GHC.Generics (Generic)

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext, trans, transitionRules,
                     wrapFailed, (?!))
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (BlockCount, Slot)

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Update.Data (SIPData)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Hupdate (HUPDATE)
import qualified Cardano.Ledger.Spec.STS.Update.Hupdate as Hupdate


-- | The Block HEADER STS
data HEADER hashAlgo

data Env hashAlgo
  = Env { k :: !BlockCount }
        deriving (Eq, Show)

data St hashAlgo
 = St { currentSlot :: !Slot
      , wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
      , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
      }
      deriving (Eq, Show, Generic)

data BHeader
  = BHeader { slot :: !Slot }
   deriving (Eq, Show, Generic)


deriving instance HasTypeReps (BHeader)

instance Sized BHeader where
  -- TODO: define this properly
  costsList bh = [(typeOf bh, 100)]

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         ) => STS (HEADER hashAlgo) where

  type Environment (HEADER hashAlgo) = Env hashAlgo

  type State (HEADER hashAlgo) = St hashAlgo

  type Signal (HEADER hashAlgo) = BHeader

  data PredicateFailure (HEADER hashAlgo)
    = BlockSlotNotIncreasing Slot Slot
    | HeaderFailure (PredicateFailure (HUPDATE hashAlgo))
    deriving (Eq, Show)


  initialRules = [ ]

  transitionRules = [
    do
      TRC ( Env { k }
          , St  { currentSlot
                , wrsips
                , asips
                }
          , BHeader { slot }
          ) <- judgmentContext

      currentSlot < slot
        ?! BlockSlotNotIncreasing currentSlot slot
      Hupdate.St { Hupdate.wrsips = wrsips'
                 , Hupdate.asips = asips'
                 } <- trans @(HUPDATE hashAlgo)
                      $ TRC ( Hupdate.Env { Hupdate.k = k
                                          , Hupdate.currentSlot = slot
                                          }
                            , Hupdate.St { Hupdate.wrsips = wrsips
                                         , Hupdate.asips = asips
                                         }
                            , slot
                            )

      pure $ St { currentSlot = slot
                , wrsips = wrsips'
                , asips = asips'
                }
    ]

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         )
  => Embed (HUPDATE hashAlgo) (HEADER hashAlgo)
    where
      wrapFailed = HeaderFailure
