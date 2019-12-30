{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Ledger.Spec.Classes.IsVoteForSU where

import Data.Kind (Type)

import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.HasSigningScheme
                    (Signature, VKey)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Data (Confidence)
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.IsSU (IsSU, SUHasHash, SUHash)
import           Cardano.Ledger.Spec.State.RevealedSUs (RevealedSUs)
import           Cardano.Ledger.Spec.Classes.Indexed ((!))

-- | A vote for a software update
class (Hashable p, IsSU u p, SUHasHash u p) => IsVoteForSU u p  where
  type IsVote u p :: Type
  votedSUHash :: (IsVote u p) -> (SUHash u p)
  confidenceSU :: (IsVote u p) -> Data.Confidence
  voterSU :: (IsVote u p) -> VKey p
  voterSigSU :: (IsVote u p) -> (Signature p ((SUHash u p), Confidence, VKey p))
  -- | Return in how many slots the voting period of the specific @SU@ will end.
  -- The result is based on the voting period duration recorded in the @SU@ metadata
  -- ('Data.SIPMetadata', 'Data.UPMetadata').
  votingPeriodEnd :: (Ord (Data.UPHash p)) => SUHash u p -> RevealedSUs u p -> Core.SlotCount

instance (Hashable p) => IsVoteForSU (Data.SIP p) p where
  type IsVote (Data.SIP p) p = Data.VoteForSIP p
  votedSUHash = Data.votedSIPHash
  confidenceSU = Data.confidenceSIP
  voterSU = Data.voterSIP
  voterSigSU = Data.voterSigSIP
  votingPeriodEnd suHash sudb = Data.votPeriodDuration . Data.metadata . Data.payloadSIP $ sudb!suHash

instance (Hashable p) => IsVoteForSU (Data.UP p) p where
  type IsVote (Data.UP p) p = Data.VoteForUP p
  votedSUHash = Data.votedUPHash
  confidenceSU = Data.confidenceUP
  voterSU = Data.voterUP
  voterSigSU = Data.voterSigUP
  votingPeriodEnd suHash sudb = Data.votPeriodDurationUP . Data.metadataUP . Data.payloadUP $ sudb!suHash
