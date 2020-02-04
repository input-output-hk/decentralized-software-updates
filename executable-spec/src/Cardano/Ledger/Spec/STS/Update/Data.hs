{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Spec.STS.Update.Data where

import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (typeOf)
import           Data.Word (Word64, Word8)
import           GHC.Generics (Generic)
import           System.Random (Random)

import           Cardano.Binary (ToCBOR (toCBOR), encodeInt)

import           Data.AbstractSize (HasTypeReps, typeReps)

import           Cardano.Ledger.Spec.STS.Sized (Sized, costsList)

data ImplementationPayload = ImplementationPayload
  deriving (Eq, Show, Generic, HasTypeReps)

-- | Vote Confidence with a 3-valued logic
data Confidence = For | Against | Abstain
  deriving (Eq, Ord, Show, Enum, Generic, HasTypeReps)

-- | Records the voting result for a specific software update (SIP/UP)
data VotingResult =
  VotingResult { stakeInFavor :: !Stake
               , stakeAgainst :: !Stake
               , stakeAbstain :: !Stake
               , rvNoQuorum :: Word8
               -- ^ No quorum revoting : how many times revoting has taken place
               -- due to a no quorum result
               --
               -- TODO: this should be a newtype
              , rvNoMajority :: Word8
               -- ^ No majority revoting : how many times revoting has taken
               -- place due to a no majority result
               --
               -- TODO: this should be a newtype
               }
  deriving (Eq, Ord, Show)

addVote :: Stake -> Confidence -> VotingResult -> VotingResult
addVote
  stake
  confidence
  votingResult@VotingResult { stakeInFavor, stakeAgainst, stakeAbstain }
  =
  case confidence of
    For -> votingResult { stakeInFavor = stakeInFavor + stake}
    Against -> votingResult { stakeAgainst = stakeAgainst + stake}
    Abstain -> votingResult { stakeAbstain = stakeAbstain + stake}

data TallyOutcome = Approved | Rejected | NoQuorum | NoMajority | Expired
  deriving (Eq, Ord, Show)

-- | Returns the stake percent as a rounded value
-- in the range [0,100]
stakePercentRound
 :: Stake
 -> Stake -- ^ Stake total
 -> Word8
stakePercentRound st totSt =
  round @Float $ fromIntegral st / fromIntegral totSt * 100

-- | Stake
newtype Stake = Stake { getStake :: Word64 }
 deriving newtype (Eq, Ord, Show, Enum, Num, Integral, Real, Random)

-- | Voting Period status values
data VPStatus = VPOpen | VPClosed
  deriving (Eq, Ord, Show)

newtype URL = URL { getText :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR)

--------------------------------------------------------------------------------
-- HasTypeReps instances
--------------------------------------------------------------------------------

-- | This instance returns one 'Char' per-each character in the URL.
instance HasTypeReps URL where
  typeReps (URL text)
    = Seq.fromList
    $ typeOf (undefined :: URL)
      : replicate (T.length text) (typeOf (undefined :: Char))

--------------------------------------------------------------------------------
-- Sized instances
--------------------------------------------------------------------------------

instance Sized ImplementationPayload where
  costsList implementationPayload = [(typeOf implementationPayload, 10)]

--------------------------------------------------------------------------------
-- ToCBOR instances
--------------------------------------------------------------------------------

instance ToCBOR Confidence where
  toCBOR = encodeInt . fromEnum
