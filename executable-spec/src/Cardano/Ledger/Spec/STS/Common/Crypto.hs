{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Common.Crypto where

import           Data.Either (isRight)
import qualified Codec.CBOR.Write as CBOR.Write
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL

import           Cardano.Binary (ToCBOR, toCBOR)
import           Cardano.Crypto.DSIGN.Class (SignedDSIGN)
import qualified Cardano.Crypto.DSIGN.Class as Crypto.DSIGN
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Crypto.DSIGN.Mock (SignKeyDSIGN (SignKeyMockDSIGN),
                     VerKeyDSIGN (VerKeyMockDSIGN))
import qualified Cardano.Crypto.DSIGN.Mock as Crypto.Mock

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     SKey, Signable, Signature, VKey, sign, verify)
import qualified Crypto.Hash as Crypto

--------------------------------------------------------------------------------
-- Hashing, signing, and verification algorithms to be used in the benchmarks
--------------------------------------------------------------------------------

data BenchCrypto

-- deriving instance ( ToCBOR (Crypto.Digest Crypto.Blake2b_224)
--                   ) => ToCBOR BenchCrypto

instance Hashable BenchCrypto where

  newtype Hash BenchCrypto a = BenchHash (Crypto.Digest Crypto.Blake2b_224)
    deriving (Eq, Ord, Show, ToCBOR)

  type HasHash BenchCrypto = ToCBOR

  -- Calculate the hash as it is done in Byron. See @module
  -- Cardano.Chain.Common.AddressHash@ in @cardano-ledger@.
  --
  hash = BenchHash . Crypto.hash . firstHash
    where
      firstHash :: ToCBOR a => a -> Crypto.Digest Crypto.SHA3_256
      firstHash
        = Crypto.hash
        . BSL.toStrict
        . Builder.toLazyByteString
        . CBOR.Write.toBuilder
        . toCBOR

instance HasSigningScheme BenchCrypto where

  newtype Signature BenchCrypto a = BenchCryptoSignature (SignedDSIGN MockDSIGN a)
    deriving (Eq, Show)

  type VKey BenchCrypto = Crypto.Mock.VerKeyDSIGN MockDSIGN

  type SKey BenchCrypto = Crypto.Mock.SignKeyDSIGN MockDSIGN

  type Signable BenchCrypto = Crypto.DSIGN.Signable MockDSIGN

  sign a skey = BenchCryptoSignature $ Crypto.Mock.mockSigned a skey

  verify vkey a (BenchCryptoSignature sig) =
    isRight $ Crypto.DSIGN.verifySignedDSIGN @MockDSIGN () vkey a sig

