{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Cardano.Ledger.Spec.Classes.IsSU where

import Data.Kind (Type)

import           Cardano.Ledger.Spec.Classes.HasSigningScheme
                    (Signature, VKey)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Data (Confidence)
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)

-- | Typeclass to define a Software Update
class (Hashable p) => IsSU u p | u -> p  where
  type SU u p :: Type

  authorSU :: u -> VKey p
  saltSU :: u -> Int


instance (Hashable p) => IsSU (Data.SIP p) p where
  type SU (Data.SIP p) p = Data.SIP p

  authorSU = Data.authorSIP
  saltSU = Data.saltSIP

instance (Hashable p) => IsSU (Data.UP p) p where
  type SU (Data.UP p) p = Data.UP p

  authorSU = Data.authorUP
  saltSU = Data.saltUP

-- | The contents of a software update
class (Hashable p, IsSU u p) => SUHasData u p | u -> p where
  type SUData u p :: Type
  dataSU :: u -> (SUData u p)

instance (Hashable p) => SUHasData (Data.SIP p) p where
  type SUData (Data.SIP p) p = Data.SIPData
  dataSU = Data.payloadSIP

instance (Hashable p) => SUHasData (Data.UP p) p where
  type SUData (Data.UP p) p = Data.UPData p
  dataSU = Data.payloadUP

-- | The metadata of a software update
class (Hashable p, IsSU u p) => SUHasMetadata u p | u -> p where
  type SUMetadata u p :: Type
  metadataSU :: u -> (SUMetadata u p)

instance (Hashable p) => SUHasMetadata (Data.SIP p) p where
  type SUMetadata (Data.SIP p) p = Data.SIPMetadata
  metadataSU = Data.metadata . Data.payloadSIP

instance (Hashable p) => SUHasMetadata (Data.UP p) p where
  type SUMetadata (Data.UP p) p = Data.UPMetadata p
  metadataSU = Data.metadataUP . Data.payloadUP

-- | The (hash) id of a software update
class (Hashable p, IsSU u p) => SUHasHash u p | u -> p where
  type SUHash u p :: Type
  hashSU :: u -> SUHash u p

instance (Hashable p) => SUHasHash (Data.SIP p) p where
  type SUHash (Data.SIP p) p = Data.SIPHash p
  hashSU = Data.hashSIP

instance (Hashable p) => SUHasHash (Data.UP p) p where
  type SUHash (Data.UP p) p = Data.UPHash p
  hashSU = Data.hashUP

-- | A vote for a software update
class (Hashable p, IsSU u p, SUHasHash u p) => IsVoteForSU u p | u -> p where
  type IsVote u p :: Type
  votedSUHash :: (IsVote u p) -> (SUHash u p)
  confidenceSU :: (IsVote u p) -> Data.Confidence
  voterSU :: (IsVote u p) -> VKey p
  voterSigSU :: (IsVote u p) -> (Signature p ((SUHash u p), Confidence, VKey p))

instance (Hashable p) => IsVoteForSU (Data.SIP p) p where
  type IsVote (Data.SIP p) p = Data.VoteForSIP p
  votedSUHash = Data.votedSIPHash
  confidenceSU = Data.confidenceSIP
  voterSU = Data.voterSIP
  voterSigSU = Data.voterSigSIP

instance (Hashable p) => IsVoteForSU (Data.UP p) p where
  type IsVote (Data.UP p) p = Data.VoteForUP p
  votedSUHash = Data.votedUPHash
  confidenceSU = Data.confidenceUP
  voterSU = Data.voterUP
  voterSigSU = Data.voterSigUP
