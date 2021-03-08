{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Update.Properties.CBORSerialisation where

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Test.QuickCheck (Property, counterexample, (===))

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))

import           Test.Cardano.Ledger.Update.Interface (updateSt)

import           Test.Cardano.Ledger.Update.Properties.SimpleScenario (Simple)
import           Test.Cardano.Ledger.Update.Properties.UpdateSUT (UpdateSUT,
                     unUpdateSt)
import           Trace.PropertyTesting (forAllTracesShow)

import qualified Trace


statesAreCorrectlyEncoded :: Property
statesAreCorrectlyEncoded =
  forAllTracesShow @UpdateSUT @Simple
      (roundtrips . updateSt . unUpdateSt . Trace.lastState ) (const "")

-- todo: this should be part of Cardano Binary, and generalized to work with
-- Hedgehog and QuickCheck.
roundtrips ::
  forall t.
  ( Eq t,
    Show t,
    ToCBOR t,
    FromCBOR t
  ) =>
  t ->
  Property
roundtrips x =
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
