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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}


module Cardano.Ledger.Spec.STS.Common.Crypto where

import           Data.Either (isRight)
import qualified Codec.CBOR.Write as CBOR.Write
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL

import qualified Cardano.Prelude as CP

import           Cardano.Binary (FromCBOR(..), ToCBOR(..), serialize', decodeFull')
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
import qualified Data.ByteString.Short as BSS (fromShort, toShort)
import Data.ByteString.Short (ShortByteString)

--------------------------------------------------------------------------------
-- Hashing, signing, and verification algorithms to be used in the benchmarks
--------------------------------------------------------------------------------

data BenchCrypto

-- deriving instance ( ToCBOR ShortByteString)
--                   => ToCBOR BenchCrypto

-- instance ToCBOR BenchCrypto where
--   toCBOR (BenchHash (Crypto.Digest Crypto.Blake2b_224)) = toCBOR (Crypto.Digest Crypto.Blake2b_224)


instance (CP.Typeable a) => FromCBOR (Hash BenchCrypto a) where
  fromCBOR = BenchHash . BSS.toShort <$> fromCBOR

instance (CP.Typeable a) => ToCBOR (Hash BenchCrypto a) where
  toCBOR (BenchHash sbs) = toCBOR (BSS.fromShort sbs)

instance  Hashable BenchCrypto where

  newtype Hash BenchCrypto a = BenchHash ShortByteString -- BenchHash (Crypto.Digest Crypto.Blake2b_224)
    deriving (Eq, Ord, Show)
    

  type HasHash BenchCrypto = ToCBOR

  -- Calculate the hash as it is done in Byron. See @module
  -- Cardano.Chain.Common.AddressHash@ in @cardano-ledger@.
  --
  hash a = BenchHash $ BSS.toShort (serialize' a)
  -- hash :: (HasHash BenchCrypto a, ToCBOR (Crypto.Digest a), Crypto.HashAlgorithm a) 
  --      => a -> Hash BenchCrypto a
  -- hash a =   toBenchHash $ (Crypto.hash . firstHash $ a) -- BenchHash . Crypto.hash . firstHash
  --   where
  --     firstHash :: ToCBOR a => a -> Crypto.Digest Crypto.SHA3_256
  --     firstHash
  --       = Crypto.hash
  --       . BSL.toStrict
  --       . Builder.toLazyByteString
  --       . CBOR.Write.toBuilder
  --       . toCBOR

-- toBenchHash :: (ToCBOR (Hash p a), CP.Typeable a) =>  Hash p a -> Hash BenchCrypto a
-- toBenchHash hsh =
--   BenchHash (BSS.toShort (serialize' hsh))

-- toBenchHash :: (ToCBOR (Crypto.Digest a), CP.Typeable a) =>  Crypto.Digest a -> Hash BenchCrypto a
-- toBenchHash hsh =
--   BenchHash (BSS.toShort (serialize' hsh))


instance HasSigningScheme BenchCrypto where

  newtype Signature BenchCrypto a = BenchCryptoSignature (SignedDSIGN MockDSIGN a)
    deriving (Eq, Show)

  type VKey BenchCrypto = Crypto.Mock.VerKeyDSIGN MockDSIGN

  type SKey BenchCrypto = Crypto.Mock.SignKeyDSIGN MockDSIGN

  type Signable BenchCrypto = Crypto.DSIGN.Signable MockDSIGN

  sign a skey = BenchCryptoSignature $ Crypto.Mock.mockSigned a skey

  verify vkey a (BenchCryptoSignature sig) =
    isRight $ Crypto.DSIGN.verifySignedDSIGN @MockDSIGN () vkey a sig

