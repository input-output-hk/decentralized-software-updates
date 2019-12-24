{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

-- See Cardano.Ledger.Spec.State.ActiveSIPs
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Spec.State.RevealedSUs where

import           Data.Map.Strict (Map)
import qualified Data.Typeable as T

import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import qualified Ledger.Core as Core

import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)
import           Cardano.Ledger.Spec.Classes.Indexed (Indexed, (!))
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey)

-- | SIP registered in the system.
newtype RevealedSUs p d = RevealedSUs (Map (Data.SUHash p d) (Data.SU p d))
  deriving newtype (Core.Relation, Semigroup, Monoid, Indexed)
--  deriving stock (Show)

deriving instance (Hashable p, Hashable d, Eq d, Eq (VKey p)) => Eq (RevealedSUs p d)
deriving instance (Hashable p, Hashable d, Show d, Show (VKey p)) => Show (RevealedSUs p d)

-- | Return in how many slots the voting period of the specific @SU@ will end.
-- The result is based on the voting period duration recorded in the @SU@ metadata
-- ('Data.SIPMetadata', 'Data.UPMetadata').
votingPeriodEnd
  :: (Hashable p, HasVotingPeriod d)
  => forall p d.
     Data.SUHash p d
  -> RevealedSUs p d
  -> Core.SlotCount
votingPeriodEnd suHash sudb
  = case (T.cast $ Data.metadataSU . Data.payloadSU $ sudb!suHash) of
      Just (mdSIP :: Data.SIPMetadata) -> Data.votPeriodDuration mdSIP
      Nothing ->
        case (T.cast $ Data.metadataSU . Data.payloadSU $ sudb!suHash) of
          Just (mdUP :: (Data.UPMetadata p)) -> Data.votPeriodDurationUP mdUP
          Nothing -> error "Cardano.Ledger.Spec.State.RevealedSUs.votingPeriodEnd: wrong type of metadata"

  -- = let
  --     metadata :: (T.Typeable m) => m
  --     metadata = Data.metadataSU . Data.payloadSU $ sudb!suHash
  --   in case (T.cast metadata) of
  --        Just (mdSIP :: Data.SIPMetadata) -> Data.votPeriodDuration mdSIP
  --        Nothing ->
  --          case (T.cast metadata) of
  --            Just (mdUP :: (Data.UPMetadata p)) -> Data.votPeriodDurationUP mdUP
  --            Nothing -> error "Cardano.Ledger.Spec.State.RevealedSUs.votingPeriodEnd: wrong type of metadata"


