{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Mock where

import           Data.Typeable (typeOf)

import           Cardano.Binary (ToCBOR)
import qualified Cardano.Crypto.Hash.Class as Cardano.Crypto
import           Cardano.Crypto.Hash.Short (ShortHash)

import           Data.AbstractSize (HasTypeReps, typeReps)

import           Cardano.Ledger.Spec.Classes.Hashable (HasHash, Hash, Hashable,
                     hash)
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
