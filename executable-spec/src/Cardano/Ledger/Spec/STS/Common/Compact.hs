{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}


module Cardano.Ledger.Spec.STS.Common.Compact where

import           Data.Text (pack)

import qualified Cardano.Prelude as CP
import Cardano.Binary (FromCBOR(..), ToCBOR(..), serialize', decodeFull')
import qualified Data.ByteString.Short as BSS (fromShort, toShort)
import Data.ByteString.Short (ShortByteString)

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)

--------------------------------------------------------------------------------
-- Compact Hash
--------------------------------------------------------------------------------

-- | A compact in-memory representation for a `Hash`
--
-- Convert using 'toCompactHash' and 'fromCompactHash'.
--
newtype CompactHash = CompactHash ShortByteString
  deriving (Eq, Ord, CP.Generic, Show)
  deriving newtype CP.HeapWords
  deriving anyclass CP.NFData
  deriving CP.NoUnexpectedThunks via CP.UseIsNormalForm ShortByteString

instance FromCBOR CompactHash where
  fromCBOR = CompactHash . BSS.toShort <$> fromCBOR

instance ToCBOR CompactHash where
  toCBOR (CompactHash sbs) = toCBOR (BSS.fromShort sbs)

toCompactHash :: (ToCBOR (Hash p a), CP.Typeable a) =>  Hash p a -> CompactHash
toCompactHash hsh =
  CompactHash (BSS.toShort (serialize' hsh))

fromCompactHash :: FromCBOR (Hash p a) => CompactHash -> Hash p a
fromCompactHash (CompactHash hsh) =
  case decodeFull' (BSS.fromShort hsh) of
    Left err      -> CP.panic $ pack $ ("fromCompactHash: impossible: " <> show err)
    Right decHash -> decHash
