{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.Classes.IsSUCommit where

import Data.Kind (Type)

import           Cardano.Ledger.Spec.Classes.HasSigningScheme
                    (Signature, VKey)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable, HasHash, Hash)

import           Cardano.Ledger.Spec.Classes.IsSU (IsSU, SU)

-- | A software update commitment
class (Hashable p) => IsSUCommit u p where
  type SUCommit u p :: Type

  authorSUcom :: SUCommit u p -> VKey p

instance (Hashable p) => IsSUCommit (Data.SIPCommit p) p where
  type SUCommit (Data.SIPCommit p) p = Data.SIPCommit p

  authorSUcom = Data.authorSIPcom

instance (Hashable p) => IsSUCommit (Data.UPCommit p) p where
  type SUCommit (Data.UPCommit p) p = Data.UPCommit p

  authorSUcom = Data.authorUPcom

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

  hashSUCommit = Data.commitSIP
  sigSUcom = Data.sigSIP
  calcCommitSU = Data.calcCommit

instance ( Hashable p
         , HasHash p (Data.UP p)
         , HasHash p (Int, VKey p, Hash p (Data.UP p))
         ) => SUCommitHasHash (Data.UPCommit p) p (Data.UP p) where
  type CommitSU (Data.UPCommit p) p = Data.CommitUP p

  hashSUCommit = Data.commitUP
  sigSUcom = Data.sigUP
  calcCommitSU = Data.calcCommitUP
