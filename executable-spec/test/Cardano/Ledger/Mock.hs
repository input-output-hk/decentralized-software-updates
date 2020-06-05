{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Mock where

import           Cardano.Binary (ToCBOR)
import           Cardano.Crypto.DSIGN.Class (SignedDSIGN)
import qualified Cardano.Crypto.DSIGN.Class as Crypto.DSIGN
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN,
                     SignKeyDSIGN (SignKeyMockDSIGN))
import qualified Cardano.Crypto.DSIGN.Mock as Crypto.Mock
import qualified Cardano.Crypto.Hash.Class as Cardano.Crypto
import           Cardano.Crypto.Hash.Short (ShortHash)
import           Data.Either (isRight)

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
import           Cardano.Ledger.Spec.Classes.HasSigningScheme (HasSigningScheme,
                     SKey, Signable, Signature, VKey, sign, verify)


data Mock

instance Hashable Mock where

  newtype Hash Mock a = MockHash (Cardano.Crypto.Hash ShortHash a)
    deriving (Eq, Ord, ToCBOR)

  type HasHash Mock = ToCBOR

  hash = MockHash . Cardano.Crypto.hash

instance Show (Hash Mock a) where
  -- We need to enclose 'theHash' in quotation marks since otherwise GHC cannot
  -- parse the printed value, which is quite important for pretty printing test
  -- output.
  show (MockHash theHash) = "MockHash \"" ++ show theHash ++ "\""

instance HasSigningScheme Mock where

  newtype Signature Mock a = MockSignature (SignedDSIGN MockDSIGN a)
    deriving (Eq, Show)

  type VKey Mock = Crypto.Mock.VerKeyDSIGN MockDSIGN

  type SKey Mock = Crypto.Mock.SignKeyDSIGN MockDSIGN

  type Signable Mock = Crypto.DSIGN.Signable MockDSIGN

  sign a skey = MockSignature $ Crypto.Mock.mockSigned a skey

  verify vkey a (MockSignature sig) = isRight $ Crypto.DSIGN.verifySignedDSIGN @MockDSIGN () vkey a sig

vkeyFromSkey :: SKey Mock -> VKey Mock
vkeyFromSkey (Crypto.Mock.SignKeyMockDSIGN i) = Crypto.Mock.VerKeyMockDSIGN i

wordToSKey :: Word -> SKey Mock
wordToSKey = SignKeyMockDSIGN . fromIntegral
