{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Test.Mock where

import           Data.Typeable (typeOf)

import           Cardano.Binary (ToCBOR)
import           Cardano.Crypto.DSIGN.Class (SignedDSIGN)
import qualified Cardano.Crypto.DSIGN.Class as Crypto.DSIGN
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import qualified Cardano.Crypto.DSIGN.Mock as Crypto.Mock
import qualified Cardano.Crypto.Hash.Class as Cardano.Crypto
import           Cardano.Crypto.Hash.Short (ShortHash)
import           Data.Either (isRight)

import           Data.AbstractSize (HasTypeReps, typeReps)

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     SKey, Signable, Signature, VKey, sign, verify)
import           Cardano.Ledger.Spec.Classes.Sizeable (HasSize, Size, Sizeable,
                     size)
import qualified Cardano.Ledger.Spec.STS.Sized as Sized

data Mock

instance Hashable Mock where

  newtype Hash Mock a = MockHash (Cardano.Crypto.Hash ShortHash a)
    deriving (Eq, Ord, Show, ToCBOR, HasTypeReps)

  type HasHash Mock = ToCBOR

  hash = MockHash . Cardano.Crypto.hash

instance Sizeable Mock where

  newtype Size Mock = MockSize Sized.Size
    deriving (Eq, Ord, Num, Show)

  type HasSize Mock = Sized.Sized

  size = MockSize . Sized.size

instance HasTypeReps Mock where
  typeReps x = [typeOf x]

instance HasSigningScheme Mock where

  newtype Signature Mock a = MockSignature (SignedDSIGN MockDSIGN a)
    deriving (Eq, Show)
    deriving newtype (HasTypeReps)

  type VKey Mock = Crypto.Mock.VerKeyDSIGN MockDSIGN

  type SKey Mock = Crypto.Mock.SignKeyDSIGN MockDSIGN

  type Signable Mock = Crypto.DSIGN.Signable MockDSIGN

  sign a skey = MockSignature $ Crypto.Mock.mockSigned a skey

  verify vkey a (MockSignature sig) = isRight $ Crypto.DSIGN.verifySignedDSIGN @MockDSIGN () vkey a sig
