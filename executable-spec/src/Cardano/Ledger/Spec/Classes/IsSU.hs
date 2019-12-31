{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Cardano.Ledger.Spec.Classes.IsSU where

import Data.Kind (Type)

import           Ledger.Core (SlotCount)

import           Cardano.Ledger.Spec.Classes.HasSigningScheme
                    (VKey)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)

-- | Typeclass to define a Software Update
class (Hashable p) => IsSU u p where
  type SU u p :: Type

  authorSU :: SU u p -> VKey p
  saltSU :: SU u p -> Int

instance (Hashable p) => IsSU (Data.SIP p) p where
  type SU (Data.SIP p) p = Data.SIP p

  authorSU = Data.authorSIP
  saltSU = Data.saltSIP

instance (Hashable p) => IsSU (Data.UP p) p where
  type SU (Data.UP p) p = Data.UP p

  authorSU = Data.authorUP
  saltSU = Data.saltUP

-- | The contents of a software update
class (Hashable p, IsSU u p) => SUHasData u p where
  type SUData u p :: Type
  dataSU :: SU u p -> (SUData u p)

instance (Hashable p) => SUHasData (Data.SIP p) p where
  type SUData (Data.SIP p) p = Data.SIPData
  dataSU = Data.payloadSIP

instance (Hashable p) => SUHasData (Data.UP p) p where
  type SUData (Data.UP p) p = Data.UPData p
  dataSU = Data.payloadUP

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

instance (Hashable p) => SUHasHash (Data.SIP p) p where
  type SUHash (Data.SIP p) p = Data.SIPHash p
  hashSU = Data.hashSIP

instance (Hashable p) => SUHasHash (Data.UP p) p where
  type SUHash (Data.UP p) p = Data.UPHash p
  hashSU = Data.hashUP

