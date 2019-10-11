{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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


import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), IRC (IRC), initialRules,
                     judgmentContext, transitionRules)
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot, BlockCount)
import           Ledger.Core ((▷>=), (▷<=), (-.), (*.))


import           Cardano.Ledger.Spec.STS.Update.Data ( SIPData
                                                     , Commit
                                                     )
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data



-- | The Header Update STS
-- Incorporates "update logic" processing
-- at the block header level
data HUPDATE hashAlgo

data Env
 = Env { k :: !BlockCount
         -- ^ Chain stability parameter.
       , currentSlot :: !Slot
       }
       deriving (Eq, Show)

data St hashAlgo
  = St { wrsips :: !(Map (Data.SIPHash hashAlgo) Slot)
       -- ^ When a SIP was revealed
       , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
       -- ^ When a SIP will not be active any more
       -- (i.e., end of open for voting period)
       }
       deriving (Eq, Show)

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         , HasTypeReps (Commit hashAlgo)
         ) => STS (HUPDATE hashAlgo) where

  type Environment (HUPDATE hashAlgo) = Env

  type State (HUPDATE hashAlgo) = St hashAlgo

  type Signal (HUPDATE hashAlgo) = Slot

  data PredicateFailure (HUPDATE hashAlgo)
    = ErrorOnHUpdate Slot Slot
    deriving (Eq, Show)


  initialRules = [
    do
      IRC Env { currentSlot } <- judgmentContext
      pure $! St { wrsips = Map.empty
                 , asips = Map.empty
                 }
    ]

  transitionRules = [
    do
      TRC ( env@Env { k, currentSlot }
          , st@St  { wrsips
                , asips
                }
          , slot
          ) <- judgmentContext

      let -- exclude old revealed SIPs
          wrsips' = wrsips ▷>= (slot -. (2 *. k))
          -- add newely revealed (but stable) SIPs to the active sips
          asips' = Map.union -- map.union is left biased
                             -- in case of dublicates
                    (wrsips ▷<= (slot -. (2 *. k)))
                    asips

      pure $ St { wrsips = wrsips'
                , asips = asips'
                }
    ]



