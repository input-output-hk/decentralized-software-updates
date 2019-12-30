{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Cardano.Ledger.Spec.Classes.IsSUCommit where

import Data.Kind (Type)

import           Cardano.Ledger.Spec.Classes.HasSigningScheme
                    (Signature, VKey)
import qualified Cardano.Ledger.Spec.STS.Update.Data as Data
import           Cardano.Ledger.Spec.Classes.Hashable (Hashable)

-- | A software update commitment
class (Hashable p) => IsSUCommit u p where
  type SUCommit u p :: Type

  authorSUcom :: u -> VKey p


instance (Hashable p) => IsSUCommit (Data.SIPCommit p) p where
  type SUCommit (Data.SIPCommit p) p = Data.SIPCommit p

  authorSUcom = Data.authorSIPcom

instance (Hashable p) => IsSUCommit (Data.UPCommit p) p where
  type SUCommit (Data.UPCommit p) p = Data.UPCommit p

  authorSUcom = Data.authorUPcom

-- | The (hash) id of a software update commitment
class (Hashable p, IsSUCommit u p) => SUCommitHasHash u p where
  type CommitSU u p :: Type

  hashSUCommit :: u -> CommitSU u p
  sigSUcom :: u -> (Signature p (CommitSU u p))

instance (Hashable p) => SUCommitHasHash (Data.SIPCommit p) p where
  type CommitSU (Data.SIPCommit p) p = Data.CommitSIP p

  hashSUCommit = Data.commitSIP
  sigSUcom = Data.sigSIP

instance (Hashable p) => SUCommitHasHash (Data.UPCommit p) p where
  type CommitSU (Data.UPCommit p) p = Data.CommitUP p

  hashSUCommit = Data.commitUP
  sigSUcom = Data.sigUP
