{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Ledger.Update.Properties.CBORSerialisation where

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Test.QuickCheck

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))

-- todo: this should be part of Cardano Binary, and generalized to work with
-- Hedgehog and QuickCheck.
roundtrip ::
  forall t.
  ( Eq t,
    Show t,
    ToCBOR t,
    FromCBOR t
  ) =>
  t ->
  Property
roundtrip x =
  case fromCBORtoCBOR x of
    Right (remaining, y) | BSL.null remaining -> x === y
    Right (remaining, _) ->
      counterexample
        ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
        False
    Left stuff ->
      counterexample
        ("Failed to decode: " <> show stuff)
        False
  where
    fromCBORtoCBOR = deserialiseFromBytes fromCBOR
                   . toLazyByteString
                   . toCBOR
