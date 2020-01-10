{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Cardano.Ledger.Spec.Classes.IsSU where

import           Data.AbstractSize (HasTypeReps, typeReps)
import           Data.Kind (Type)

import           Ledger.Core (SlotCount)

import           Cardano.Ledger.Spec.Classes.HasSigningScheme
                    (VKey)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.STS.Update.Data (SIP, UP)
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.State.ApprovedSIPs (ApprovedSIPs, isSIPApproved)

-- | Typeclass to define a Software Update
class (Hashable p) => IsSU u p where
  data SU u p :: Type

  authorSU :: SU u p -> VKey p
  saltSU :: SU u p -> Int

instance (Hashable p) => IsSU (SIP p) p where
  newtype SU (SIP p) p = SUSIP (SIP p)

  authorSU (SUSIP sip) = Data.authorSIP sip
  saltSU (SUSIP sip) = Data.saltSIP sip

instance (Hashable p) => IsSU (UP p) p where
  newtype SU (UP p) p = SUUP (UP p)

  authorSU (SUUP up) = Data.authorUP up
  saltSU (SUUP up) = Data.saltUP up

-- | The contents of a software update
class (Hashable p, IsSU u p) => SUHasData u p where
  type SUData u p :: Type
  dataSU :: SU u p -> (SUData u p)

instance (Hashable p) => SUHasData (Data.SIP p) p where
  type SUData (Data.SIP p) p = Data.SIPData
  dataSU (SUSIP sip) = Data.payloadSIP sip

instance (Hashable p) => SUHasData (Data.UP p) p where
  type SUData (Data.UP p) p = Data.UPData p
  dataSU (SUUP up) = Data.payloadUP up

-- | The metadata of a software update
class (Hashable p, IsSU u p) => SUHasMetadata u p where
  type SUMetadata u p :: Type
  metadataSU :: SUData u p -> SUMetadata u p
  votPeriodDuration :: SUMetadata u p -> SlotCount

instance (Hashable p) => SUHasMetadata (Data.SIP p) p where
  type SUMetadata (Data.SIP p) p = Data.SIPMetadata
  metadataSU = Data.metadata
  votPeriodDuration = Data.votPeriodDuration

instance (Hashable p) => SUHasMetadata (Data.UP p) p where
  type SUMetadata (Data.UP p) p = Data.UPMetadata p
  metadataSU = Data.metadataUP
  votPeriodDuration = Data.votPeriodDurationUP

-- | The (hash) id of a software update
class (Hashable p, IsSU u p) => SUHasHash u p where
  type SUHash u p :: Type
  hashSU :: SU u p -> SUHash u p
  isSUApproved :: SUMetadata u p -> ApprovedSIPs p -> Bool

instance (Hashable p) => SUHasHash (Data.SIP p) p where
  type SUHash (Data.SIP p) p = Data.SIPHash p
  hashSU (SUSIP sip) = Data.hashSIP sip
  isSUApproved _ _ = True
    -- This check is not relevant to the Ideation phase

instance (Hashable p) => SUHasHash (Data.UP p) p where
  type SUHash (Data.UP p) p = Data.UPHash p
  hashSU (SUUP up) = Data.hashUP up
  isSUApproved mdata approvesips =
    let siphash = Data.sipReference mdata
    in isSIPApproved siphash approvesips

instance HasTypeReps p => HasTypeReps (SU u p) where
  typeReps _ = typeReps (undefined :: p)
