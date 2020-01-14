{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.Spec.Classes.IsSUCommit where

import           Data.Kind (Type)
import           Data.AbstractSize (HasTypeReps, typeReps)
import           Data.Typeable (Typeable)

import           Cardano.Binary (ToCBOR (toCBOR))

import           Cardano.Ledger.Spec.Classes.HasSigningScheme
                    (Signature, VKey)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable, HasHash, Hash)

import           Cardano.Ledger.Spec.Classes.IsSU (IsSU, SU, SU(SUSIP), SU(SUUP))

-- | A software update commitment
class (Hashable p) => IsSUCommit u p where
  data SUCommit u p :: Type

  authorSUcom :: SUCommit u p -> VKey p

instance (Hashable p) => IsSUCommit (Data.SIPCommit p) p where
  newtype SUCommit (Data.SIPCommit p) p = SUSIPCommit (Data.SIPCommit p)

  authorSUcom (SUSIPCommit sipcomm) = Data.authorSIPcom sipcomm

instance (Hashable p) => IsSUCommit (Data.UPCommit p) p where
  newtype SUCommit (Data.UPCommit p) p = SUUPCommit (Data.UPCommit p)

  authorSUcom (SUUPCommit upcomm) = Data.authorUPcom upcomm

-- | The (hash) id of a software update commitment
class (Hashable p, IsSUCommit u p, IsSU su p) => SUCommitHasHash u p su where
  type CommitSU u p :: Type

  hashSUCommit :: SUCommit u p -> CommitSU u p
  sigSUcom :: SUCommit u p -> (Signature p (CommitSU u p))
  -- | Calculate a `Commit` from a SU
  calcCommitSU :: SU su p -> CommitSU u p

instance ( Hashable p
         , HasHash p (Data.SIP p)
         , HasHash p (Int, VKey p, Hash p (Data.SIP p))
         ) => SUCommitHasHash (Data.SIPCommit p) p (Data.SIP p) where
  type CommitSU (Data.SIPCommit p) p = Data.CommitSIP p

  hashSUCommit (SUSIPCommit sipcomm) = Data.commitSIP sipcomm
  sigSUcom (SUSIPCommit sipcomm) = Data.sigSIP sipcomm
  calcCommitSU (SUSIP sip) = Data.calcCommit sip

instance ( Hashable p
         , HasHash p (Data.UP p)
         , HasHash p (Int, VKey p, Hash p (Data.UP p))
         ) => SUCommitHasHash (Data.UPCommit p) p (Data.UP p) where
  type CommitSU (Data.UPCommit p) p = Data.CommitUP p

  hashSUCommit (SUUPCommit upcomm) = Data.commitUP upcomm
  sigSUcom (SUUPCommit upcomm) = Data.sigUP upcomm
  calcCommitSU (SUUP up) = Data.calcCommitUP up

instance HasTypeReps p => HasTypeReps (SUCommit u p) where
  typeReps _ = typeReps (undefined :: p)


