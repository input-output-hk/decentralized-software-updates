{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Update.Properties.CBORSerialisation where

import           Codec.CBOR.Read (deserialiseFromBytes)
import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Test.QuickCheck (Gen, Property, counterexample, (===))

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))

import           SystemUnderTest (SUTSt)
import           Test.Cardano.Ledger.Update.Properties.SimpleScenario (Simple)
import           Test.Cardano.Ledger.Update.Properties.UpdateSUT (UpdateSUT)
import           Trace (Trace)
import qualified Trace
import           Trace.Generation (arbitraryTrace, shrinkTrace)
import           Trace.PropertyTesting (forAllTracesShow)


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
