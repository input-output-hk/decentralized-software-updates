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


module Cardano.Ledger.Spec.STS.Chain.Header where

import           GHC.Generics (Generic)
import           Data.Monoid.Generic (GenericMonoid (GenericMonoid),
                     GenericSemigroup (GenericSemigroup))
import           Data.Typeable (typeOf)
import qualified Data.Map.Strict as Map

import           Cardano.Crypto.Hash (Hash, HashAlgorithm)

import           Control.State.Transition (Embed, Environment, PredicateFailure,
                     STS, Signal, State, TRC (TRC), IRC (IRC), initialRules,
                     judgmentContext, trans, transitionRules, wrapFailed, (?!))
import           Data.AbstractSize (HasTypeReps)

import           Ledger.Core (Slot)

import           Cardano.Ledger.Spec.STS.Update.Hupdate (HUPDATE)
import qualified Cardano.Ledger.Spec.STS.Update.Hupdate as Hupdate
import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)
import           Cardano.Ledger.Spec.STS.Update.Data (SIPData)


-- | The Block HEADER STS
data HEADER hashAlgo

data Env hashAlgo
  = Env { initialSlot :: !Slot
        , hupdateEnv :: Environment (HUPDATE hashAlgo)
        }
        deriving (Eq, Show)

data St hashAlgo
 = St { currentSlot :: !Slot
      , hupdateSt :: State (HUPDATE hashAlgo)
      }
      deriving (Eq, Show, Generic)
      deriving Semigroup via GenericSemigroup (St hashAlgo)
      deriving Monoid via GenericMonoid (St hashAlgo)


data BHeader
  = BHeader
   { slot :: !Slot
   }
   deriving (Eq, Show, Generic)

-- deriving instance ( HasTypeReps hashAlgo
--                   , HasTypeReps (Hash hashAlgo SIPData)
--                   , HashAlgorithm hashAlgo
--                   , HasTypeReps (Commit hashAlgo)
--                   ) => HasTypeReps (BHeader)

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


  initialRules = [
    do
      IRC Env { initialSlot } <- judgmentContext
      pure $! St { currentSlot = initialSlot
                 , hupdateSt = Hupdate.St { Hupdate.wrsips = Map.empty
                                          , Hupdate.asips = Map.empty
                                          }
                 }
    ]

  transitionRules = [
    do
      TRC ( Env { hupdateEnv = Hupdate.Env { Hupdate.k
                                           , Hupdate.currentSlot = _
                                           }
                }
          , St  { currentSlot
                , hupdateSt
                }
          , BHeader { slot }
          ) <- judgmentContext

      currentSlot < slot
        ?! BlockSlotNotIncreasing currentSlot slot
      hupdateSt' <-
        trans @(HUPDATE hashAlgo)
          $ TRC ( Hupdate.Env { Hupdate.k, Hupdate.currentSlot = slot }
                , hupdateSt
                , slot
                )

      pure $ St { currentSlot = slot
                , hupdateSt = hupdateSt'
                }
    ]

instance ( HashAlgorithm hashAlgo
         , HasTypeReps hashAlgo
         , HasTypeReps (Hash hashAlgo SIPData)
         )
  => Embed (HUPDATE hashAlgo) (HEADER hashAlgo)
    where
      wrapFailed = HeaderFailure
