{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Protocol payload sizes
--
-- Sizes are calculated based on the CBOR encoding of the data types used in the
-- decentralized update system prototype.
module Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.PayloadSizes where

import           Cardano.Ledger.Benchmarks.Update.WorstCaseAnalysis.Units


-- | Size of a commit.
--
commitSize :: NumberOfBytes
commitSize
  = 33 -- commit itself: hash: tag + 32, CBOR tags are 1 byte
  + 33 -- author       : vkey: tag + 32
  + 97 -- signature    :       tag + 64 + 32

-- | Size of a reveal message.
--
revealSize :: NumberOfBytes
revealSize
  = 33 -- SIP hash: tag + 32
  + 33 -- author  : vkey
  + 9  -- salt    : int: tag + 8
  + sipDataSize
  where
    -- SIP data:
    sipDataSize
      = 2049 -- url: tag + 1 byte per-char, assume a max size of 2048
      + sipMetadataSize
      where
        -- SIP metadata:
        sipMetadataSize
          = 9 -- version from: 64 bits word: tag + 8 (max)
          + 9 -- version to: 64 bits word
          + 1 -- impact consensus: small enumeration, note that the value and
              -- CBOR tag are fit in one byte.
          + 5 -- impacts parameters: tag + 4 possible values of 1 byte each.
              -- CBOR tag and value fit in one byte.
          + 9 -- vote period duration: 64 bits word

-- | Size of a vote message.
--
voteSize :: NumberOfBytes
voteSize
  = 33 -- voted sip hash
  + 1  -- confidence: small enumeration
  + 33 -- voter: vkey
  + 97 -- signature

-- | Chain stability parameter (also called security parameter). This value is
-- the same as the one used in the Byron mainnet.
k :: NumberOfSlots
k = 2160

-- | Seconds per slot. Same as in the Byron mainnet.
secondsPerSlot :: Seconds
secondsPerSlot = 20

-- | Given a number of slots, calculate how many seconds are required for those
-- slots to pass. This depends on the 'slotsPerSecond' constant.
slotsToSeconds :: NumberOfSlots -> Seconds
slotsToSeconds numberOfSlots = fromIntegral numberOfSlots * secondsPerSlot
