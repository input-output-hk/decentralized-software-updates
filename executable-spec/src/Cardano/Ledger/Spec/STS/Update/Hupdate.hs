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
import           GHC.Generics (Generic)

import           Control.State.Transition (Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext, transitionRules)

import           Ledger.Core (BlockCount, Slot, dom, (*.), (-.), (⋪), (▷<=))

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
       , asips :: !(Map (Data.SIPHash hashAlgo) Slot)
       }
       deriving (Eq, Show, Generic)

instance  STS (HUPDATE hashAlgo) where

  type Environment (HUPDATE hashAlgo) = Env

  type State (HUPDATE hashAlgo) = St hashAlgo

  type Signal (HUPDATE hashAlgo) = Slot

  data PredicateFailure (HUPDATE hashAlgo)
    = ErrorOnHUpdate Slot Slot
    deriving (Eq, Show)


  initialRules = [
    do
      IRC Env { } <- judgmentContext
      pure $! St { wrsips = Map.empty
                 , asips = Map.empty
                 }
    ]

  transitionRules = [
    do
      TRC ( Env { k }
          , St  { wrsips
                , asips
                }
          , slot
          ) <- judgmentContext

      let
          -- Add newly revealed (but stable) SIPs to the active sips. Note that
          -- we place these new sips as arguments of the left hand side of the
          -- 'Map.union', since this operation is left biased.
          asips' = (wrsips ▷<= (slot -. (2 *. k))) `Map.union` asips
          -- exclude old revealed SIPs
          wrsips' = dom asips ⋪ wrsips

      pure $ St { wrsips = wrsips'
                , asips = asips'
                }
    ]
