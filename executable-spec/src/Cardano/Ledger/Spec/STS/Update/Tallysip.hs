{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Ledger.Spec.STS.Update.Tallysip where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           Data.AbstractSize (HasTypeReps)

import           Control.State.Transition (Embed, Environment, IRC (IRC),
                     PredicateFailure, STS, Signal, State, TRC (TRC),
                     initialRules, judgmentContext, trans, transitionRules, wrapFailed)
import qualified Ledger.Core as Core
import           Cardano.Crypto.Hash (HashAlgorithm)

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data

-- | STS for tallying the votes of a single SIP
data TALLYSIP hashAlgo

data Env hashAlgo
 = Env { ballots :: !(Map (Data.SIPHash hashAlgo) (Map Core.VKey Data.Confidence))
       }
       deriving (Eq, Show)

data St hashAlgo
  = St { vresips :: !(Map (Data.SIPHash hashAlgo) Data.VotingResult)
         -- ^ Records the current voting result for each SIP
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
        pure $! St { vresips = Map.empty }
      ]

  transitionRules = [
    do
      TRC ( Env { ballots }
          , St  { vresips }
          , sipHash
          ) <- judgmentContext

      -- do the tally
      let vresips' = vresips

      pure $ St { vresips = vresips'}
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




