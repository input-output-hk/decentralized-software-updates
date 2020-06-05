{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Data.Commit where

import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR)

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (VKey)

import           Cardano.Ledger.Spec.Classes.HasAuthor (HasAuthor, author)
import           Cardano.Ledger.Spec.Classes.HasSalt (HasSalt, salt)

-- | Commitment for a given data type.
--
newtype Commit p d =
  Commit { unCommit :: Hash p (Int, VKey p, Hash p d) }
  deriving stock (Generic)

deriving instance Hashable p => Eq (Commit p d)
deriving instance Hashable p => Ord (Commit p d)
deriving instance Hashable p => Show (Commit p d)

-- | Calculate a `Commit` from a `SIP`
calcCommit
  :: ( Hashable p
     , HasHash p d
     , HasHash p (Int, VKey p, Hash p d)
     , HasAuthor d p
     , HasSalt d
     )
  => d -> Commit p d
calcCommit d = Commit $ hash (salt d, author d, hash d)

deriving newtype instance ( Typeable p
                          , Typeable d
                          , ToCBOR (Hash p (Int, VKey p, Hash p d))
                          ) => (ToCBOR (Commit p d))
