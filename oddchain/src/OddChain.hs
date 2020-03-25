{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module OddChain where

import           Prelude hiding (flip)
import qualified Prelude

import           Codec.CBOR.Decoding as D
import           Codec.Serialise (decode, encode)
import           Control.Exception (assert)
import           Control.Monad.Except (runExcept, throwError)
import           Crypto.Random (MonadRandom)
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as Strict
import           Data.Either (isRight)
import           Data.FingerTree (Measured, measure)
import           Data.List (foldl')
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

import           Data.Proxy (Proxy (Proxy))
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Binary (DecoderError (DecoderErrorUnknownTag),
                     Encoding, FromCBOR, ToCBOR, encodeInt, encodeListLen,
                     encodeWord, fromCBOR, serialize', toCBOR)
import           Cardano.Crypto (ProtocolMagicId (ProtocolMagicId))
import           Cardano.Crypto.DSIGN.Class (SigDSIGN, SignKeyDSIGN,
                     SignedDSIGN (SignedDSIGN), VerKeyDSIGN, signDSIGN,
                     verifyDSIGN)
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN,
                     SignKeyDSIGN (SignKeyMockDSIGN),
                     VerKeyDSIGN (VerKeyMockDSIGN))
import           Cardano.Crypto.DSIGN.Mock ()
import           Cardano.Crypto.Hash.Class (hash)
import qualified Cardano.Crypto.Hash.Class as Crypto.Hash
import qualified Cardano.Crypto.Hash.Class
import           Cardano.Crypto.Hash.Short (ShortHash)
import           Cardano.Prelude (cborError)
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block (BlockSupportsProtocol, GetHeader,
                     Header, getHeader, validateView)
import           Ouroboros.Consensus.Block.Abstract (BlockConfig)
import           Ouroboros.Consensus.BlockchainTime.SlotLengths (SlotLengths,
                     SystemStart (SystemStart))
import           Ouroboros.Consensus.Config (TopLevelConfig (TopLevelConfig),
                     configBlock, configConsensus, configLedger)
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip,
                     ValidateEnvelope)
import           Ouroboros.Consensus.HeaderValidation (genesisHeaderState)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
                     (ExtLedgerState (ExtLedgerState), headerState,
                     ledgerState)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol, anachronisticProtocolLedgerView,
                     protocolLedgerView)
import           Ouroboros.Consensus.Mempool.API (ApplyTx, ApplyTxErr, GenTx,
                     HasTxId (TxId, txId), applyTx, reapplyTx, txSize)
import           Ouroboros.Consensus.Node (RunNode, nodeBlockEncodingOverhead,
                     nodeBlockFetchSize, nodeBlockMatchesHeader,
                     nodeCheckIntegrity, nodeDecodeApplyTxError,
                     nodeDecodeBlock, nodeDecodeConsensusState,
                     nodeDecodeGenTx, nodeDecodeGenTxId, nodeDecodeHeader,
                     nodeDecodeHeaderHash, nodeDecodeLedgerState,
                     nodeDecodeQuery, nodeDecodeResult, nodeDecodeTipInfo,
                     nodeDecodeWrappedHeader, nodeEncodeApplyTxError,
                     nodeEncodeBlockWithInfo, nodeEncodeConsensusState,
                     nodeEncodeGenTx, nodeEncodeGenTxId, nodeEncodeHeader,
                     nodeEncodeHeaderHash, nodeEncodeLedgerState,
                     nodeEncodeQuery, nodeEncodeResult, nodeEncodeTipInfo,
                     nodeEncodeWrappedHeader, nodeForgeBlock, nodeHashInfo,
                     nodeImmDbChunkInfo, nodeIsEBB, nodeMaxBlockSize,
                     nodeNetworkMagic, nodeProtocolMagicId, nodeStartTime)
import           Ouroboros.Consensus.Node.LedgerDerivedInfo (LedgerDerivedInfo,
                     knownSlotLengths)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
                     (HasNetworkProtocolVersion)
import           Ouroboros.Consensus.Node.ProtocolInfo (enumCoreNodes)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes)
import           Ouroboros.Consensus.Node.State (NodeState, Update)
import           Ouroboros.Consensus.NodeId (CoreNodeId (CoreNodeId),
                     NodeId (CoreId))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam,
                     ValidateView)
import           Ouroboros.Consensus.Protocol.BFT (Bft, BftDSIGN,
                     BftFields (BftFields), BftMockCrypto,
                     BftParams (BftParams), ConsensusConfig (BftConfig),
                     bftNodeId, bftNumNodes, bftParams, bftSecurityParam,
                     bftSignKey, bftValidateView, bftVerKeys)
import           Ouroboros.Consensus.Protocol.Signed (Signed,
                     SignedHeader (headerSigned))
import           Ouroboros.Consensus.Storage.Common (BinaryInfo (BinaryInfo),
                     binaryBlob, headerOffset, headerSize)
import           Ouroboros.Consensus.Storage.ImmutableDB
                     (ChunkInfo (UniformChunkSize), ChunkSize (ChunkSize))
import           Ouroboros.Consensus.Storage.ImmutableDB.Types
                     (HashInfo (HashInfo), getHash, hashSize, putHash)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import           Ouroboros.Network.Block (BlockMeasure, BlockNo,
                     ChainHash (BlockHash, GenesisHash), pattern GenesisPoint,
                     HasHeader, HeaderHash, Point, SlotNo, StandardHash,
                     blockHash, blockInvariant, blockMeasure, blockNo,
                     blockPoint, blockPrevHash, blockSlot, castHash,
                     decodePoint, encodePoint, pointHash, unSlotNo)
import           Ouroboros.Network.BlockFetch (SizeInBytes)
import           Ouroboros.Network.Magic (NetworkMagic (NetworkMagic))


-- | Block type used by the odd chain.
--
-- The consensus layer requires that we define several operations on values of
-- this type:
--
-- - getting the hash of a block
-- - getting the previous hash
-- - getting the block and slot number
--
-- See the @instance HasHeader@ for @OddBlock@ and @Header OddBlock@
--
data OddBlock
  = OddBlock
    { oddBlockHeader  :: !(Header OddBlock)
    , oddBlockPayload :: ![Tx]
    } deriving (Eq, Show, HasAnnTip, ValidateEnvelope, HasNetworkProtocolVersion)

-- | We use a very simple hash throughout this demo.
type Hash a = Crypto.Hash.Hash ShortHash a

type instance HeaderHash OddBlock = Hash (Header OddBlock)

instance StandardHash OddBlock

data instance BlockConfig OddBlock =
  OddBlockConfig
  { oddBlockConfigSlotLengths :: !SlotLengths
    -- ^ We use a constant slot length.
  }
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance Measured BlockMeasure OddBlock where
  measure = blockMeasure

instance HasHeader OddBlock where
  blockHash = blockHash . oddBlockHeader

  blockPrevHash b = castHash $ blockPrevHash (oddBlockHeader b)

  blockNo = blockNo . oddBlockHeader

  blockSlot = blockSlot . oddBlockHeader

  blockInvariant = blockInvariant . oddBlockHeader

data SignedPart
  = SignedPart
    { oddBlockIssuer      :: !(VerKeyDSIGN (BftDSIGN BftMockCrypto))
    , oddBlockPrevHash    :: !(ChainHash OddBlock)
    , oddBlockNo          :: !BlockNo
    , oddBlockSlot        :: !SlotNo
    , oddBlockPayloadHash :: !(Hash [Tx])
    , oddBlockSize        :: !SizeInBytes
    } deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance GetHeader OddBlock where
  data Header OddBlock
    = OddHeader
      { oddBlockSignedPart :: !SignedPart
      , oddBlockSignature  :: !(SigDSIGN (BftDSIGN BftMockCrypto))
        -- ^ The signed data must correspond with the 'Signed' type instance.
      } deriving (Eq, Show, Generic, NoUnexpectedThunks)

  getHeader = oddBlockHeader

instance HasHeader (Header OddBlock) where
  blockHash = hash

  blockPrevHash = castHash . oddBlockPrevHash . oddBlockSignedPart

  blockNo = oddBlockNo . oddBlockSignedPart

  blockSlot = oddBlockSlot . oddBlockSignedPart

  blockInvariant = const True

-- | An odd chain has two phases:
--
-- In the 'Decrease' phase, all numbers in a block must be smaller than the
-- numbers seen in the previous block. The number that is passed to the
-- constructor represents the upper bound on the numbers of a block.
---
-- In the 'Increase' phase, all the numbers in a block must be larger than the
-- numbers seen in the previous block. The number that is passed to the
-- constructor represents the lower bound on the numbers of a block.
--
-- The odd chain can be initialized in any of the two phases. This is, when
-- there is no previous block any phase and either lower or upper bound values
-- can be chosen.
data Phase = Decrease !Int | Increase !Int
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

flip :: Phase -> Phase
flip (Decrease i) = Increase i
flip (Increase i) = Decrease i

bump :: [Tx] -> Phase -> Phase
bump xs (Decrease i) = Decrease $ minimum $ i:(unTx <$> xs)
bump xs (Increase i) = Increase $ maximum $ i:(unTx <$> xs)

updatePhase :: Tx -> Phase -> Phase
updatePhase (Tx i) (Decrease j)
  = if i < j then Decrease i else Decrease j
updatePhase (Tx i) (Increase j)
  = if j < i then Increase i else Increase j

data OddError
  = OddError
    { currentPhase :: !Phase
      -- ^ Phase that was active at when the error occurred.
    , errors       :: ![OddTxError]
    }
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance UpdateLedger OddBlock where
  data LedgerState OddBlock
    = LedgerState
      { stLastApplied :: !(Point OddBlock)
      , phase         :: !Phase
      } deriving (Eq, Show, Generic, NoUnexpectedThunks)

  type LedgerError OddBlock = OddError

  data LedgerConfig OddBlock
    = OddConfig
    { slotsPerEpoch    :: !Word64
      -- ^ How many slots can you fit in an epoch. We use epochs to change phases.
    , cfgNodeStartTime :: !UTCTime
    } deriving (Show, Generic, NoUnexpectedThunks)

  -- Note that this function cannot fail. If there would be a state in which
  -- this function fails then no time can pass anymore. If there are ledger
  -- rules that could leave the ledger state in such state, then there must be
  -- an error in the ledger logic.
  applyChainTick cfg currentSlot st
    = TickedLedgerState
      { tickedSlotNo      = currentSlot
      , tickedLedgerState =
          st { phase = changePhaseOnEpochBoundary cfg currentSlot (phase st) }
      }

  applyLedgerBlock cfg blk@(OddBlock { oddBlockPayload }) st
  -- When defining this function, remember to update the current slot. Failure
  -- to do so will result in failed assertion, in
  -- @Ouroboros.Storage.ChainDB.Impl.ChainSel@:
  --
  -- >     assert (castPoint (AF.headPoint c) == LgrDB.currentPoint l) $
  --
  -- since the ledger will not be reporting the tip of the last applied block.
  --
    = case foldTxs cfg tickedSt oddBlockPayload of
        ([]  , tickedSt') -> do
          let st' = tickedLedgerState tickedSt'
          pure $! st' { stLastApplied = blockPoint blk
                      }
        (errs, _  ) -> throwError $ OddError
                                    { currentPhase = phase (tickedLedgerState tickedSt)
                                    , errors       = errs
                                    }
    where
      tickedSt = applyChainTick cfg (blockSlot blk) st

  -- Don't forget!: 'reapplyLedgerBlock' should apply the chain tick as well!
  reapplyLedgerBlock cfg blk@OddBlock { oddBlockPayload } st@LedgerState { phase }
    = st { stLastApplied = blockPoint blk
         , phase         = foldl' (Prelude.flip updatePhase) phase' oddBlockPayload
         }
    where
      phase' = changePhaseOnEpochBoundary cfg (blockSlot blk) phase

  ledgerTipPoint = stLastApplied

changePhaseOnEpochBoundary
  :: LedgerConfig OddBlock
  -> SlotNo
  -- ^ Current slot.
  -> Phase
  -> Phase
changePhaseOnEpochBoundary OddConfig { slotsPerEpoch } currentSlot phase
  = assert (slotsPerEpoch /= 0)
  $ if unSlotNo currentSlot `mod` slotsPerEpoch == 0
    then flip phase
    else phase

--------------------------------------------------------------------------------
-- Defining a 'ProtocolLedgerView' instance ...
--------------------------------------------------------------------------------

-- | A specific type of block should be designed for a specific kind of
-- protocol. We will design our odd blocks to run the BFT consensus algorithm.
--
-- In addition, we need to specify the BftCrypto. We will use the
-- 'BftMockCrypto'.
--
-- Note that if we wanted to be parametric over crypto we would have had to
-- define:
--
-- > data OddBlock c = ...
--
-- However, for the purpose of demo exercise this isn't necessary.
type instance BlockProtocol OddBlock = Bft BftMockCrypto

instance BlockSupportsProtocol OddBlock where

  -- Remember that
  --
  -- > type BlockConfig b = NodeConfig (BlockProtocol b)
  --
  -- And 'ValidateView' is the data required to validate the given header
  -- according to the consensus rules.
  validateView
    :: BlockConfig OddBlock
    -> Header OddBlock
    -> ValidateView (Bft BftMockCrypto)
  validateView _cfg = bftValidateView getBftFields

  -- We use the default implementation of 'selectView', which extracts the block
  -- number from the header.

getBftFields :: Header OddBlock -> BftFields BftMockCrypto (Signed (Header OddBlock))
getBftFields = BftFields . SignedDSIGN . oddBlockSignature

type instance Signed (Header OddBlock) = SignedPart

-- We need to specify how to extract the data to be signed from the block
-- header.
instance SignedHeader (Header OddBlock) where
  headerSigned = oddBlockSignedPart

instance LedgerSupportsProtocol OddBlock where

  -- The ledger view for Bft is just ().
  protocolLedgerView
    :: LedgerConfig OddBlock
    -> LedgerState OddBlock
    -> ()
  protocolLedgerView _ _ = ()

  -- The anachronistic ledger view should return the ledger view at a particular
  -- time. However, since the ledger view is just '()' we can always return it.
  anachronisticProtocolLedgerView _ _ _ = Right ()

--------------------------------------------------------------------------------
-- Defining a 'RunNode' instance ...
--------------------------------------------------------------------------------

instance ApplyTx OddBlock where
  newtype GenTx OddBlock = OddTx { unOddTx :: Tx }
    deriving (Eq, Show, Ord, Generic)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass (NoUnexpectedThunks)

  -- A transaction is a 64 bits Word.
  txSize _ = 4

  -- We use the same error for applying a transaction as for applying a block.
  -- In this case, both 'outOfBound' and 'notOdd' fields will contain at most
  -- one element, which is the integer in the transaction.
  type ApplyTxErr OddBlock = OddTxError

  applyTx _cfg (OddTx i) st@TickedLedgerState { tickedLedgerState } =
    case checkTx tickedLedgerState i of
      Nothing  ->
        pure $! st { tickedLedgerState
                       = tickedLedgerState
                         { phase = updatePhase i (phase tickedLedgerState) }
                   }
      Just err -> throwError err

  -- TODO: we should also write properties relating 'reapplyTx' and 'applyTx'.
  reapplyTx = applyTx

foldTxs
  :: LedgerConfig OddBlock
  -> TickedLedgerState OddBlock
  -> [Tx]
  -> ([OddTxError], TickedLedgerState OddBlock)
foldTxs cfg tickedSt txs  = foldl' (Prelude.flip (applyOddTx cfg)) ([], tickedSt) txs

-- | Apply a transaction to a given state. The first component of the given
-- tuple is an errors accumulator.
--
applyOddTx
  :: LedgerConfig OddBlock
  -> Tx
  -> ([OddTxError], TickedLedgerState OddBlock)
  -> ([OddTxError], TickedLedgerState OddBlock)
applyOddTx cfg tx (!errs, !st') =
  case runExcept $ applyTx cfg (OddTx tx) st' of
    Left  !err  -> (err: errs, st' )
    Right !st'' -> (     errs, st'')

checkTx :: LedgerState OddBlock -> Tx -> Maybe OddTxError
checkTx LedgerState { phase } (Tx i) =
  if odd i
  then (\obe -> OddBut obe i phase) <$> checkPhase i phase
  else Just $!  NotOdd                  (checkPhase i phase) i phase

data OddTxError
  = NotOdd !(Maybe OutOfBoundError) !Int !Phase
  | OddBut !OutOfBoundError         !Int !Phase
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

errorPhase :: OddTxError -> Phase
errorPhase (NotOdd _ _ phase) = phase
errorPhase (OddBut _ _ phase) = phase

-- | Errors caused by numbers that do not obey the ordering constraints that the
-- current phase dictates.
data OutOfBoundError = NotDecreasing | NotIncreasing
  deriving (Eq, Show, Generic, NoUnexpectedThunks, Enum)

checkPhase :: Int -> Phase -> Maybe OutOfBoundError
checkPhase i (Decrease j) =
  if i < j then Nothing else Just NotDecreasing
checkPhase i (Increase j) =
  if j < i then Nothing else Just NotIncreasing

newtype Tx = Tx { unTx :: Int }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (NoUnexpectedThunks)
  deriving newtype (ToCBOR, FromCBOR, Num, Integral, Real, Enum)

instance HasTxId (GenTx OddBlock) where

  newtype TxId (GenTx OddBlock) = TxId Int
    deriving (Eq, Ord, Show)
    deriving newtype (ToCBOR, FromCBOR, NoUnexpectedThunks)

  txId = TxId . unTx . unOddTx

instance ShowQuery (Query OddBlock) where

  showResult = \case {}

instance QueryLedger OddBlock where

  data Query OddBlock result
    deriving (Show)

  answerQuery = \case {}

  eqQuery = \case {}

-- Run node requires:
--
-- > instance ApplyTx OddBlock
-- > instance HasTxId (GenTx OddBlock)
-- > instance QueryLedger OddBlock

type instance NodeState OddBlock = ()

instance LedgerDerivedInfo OddBlock where
  knownSlotLengths = oddBlockConfigSlotLengths

instance RunNode OddBlock where

  nodeForgeBlock
    :: MonadRandom m
    => TopLevelConfig OddBlock
    -> Update m (NodeState OddBlock)
    -> SlotNo
    -- ^ Current slot
    -> BlockNo
    -- ^ Current block number
    -> ExtLedgerState OddBlock
    -- ^ Current ledger state
    -> [GenTx OddBlock]
    -- ^ Txs to add in the block
    -> ()
    -- ^ IsLeader (ExtConfig (Bft BftMockCrypto) (LedgerConfig OddBlock))
    -> m OddBlock
  nodeForgeBlock cfg _update slotNo aBlockNo extLedgerState txs () = do
    let signedPart = mkSignedPart nodeVKey prevHash aBlockNo slotNo payload
        nodeVKey   = vkeysMap ! bftNodeId (configConsensus cfg)
        vkeysMap   = bftVerKeys $ configConsensus cfg
        prevHash   = pointHash $ stLastApplied $ ledgerState $ extLedgerState
        payload    = fmap unOddTx txs
    oddSignature <- signDSIGN () signedPart (bftSignKey $ configConsensus cfg)
    let
        header     = OddHeader
                     { oddBlockSignedPart = signedPart
                     , oddBlockSignature  = oddSignature
                     }
    pure $! OddBlock
            { oddBlockHeader = header
            , oddBlockPayload = payload
            }

  nodeBlockMatchesHeader
    :: Header OddBlock -> OddBlock -> Bool
  nodeBlockMatchesHeader hdr OddBlock { oddBlockHeader } = hdr == oddBlockHeader

  -- Remember
  --
  -- > type SizeInBytes = Word
  --
  nodeBlockFetchSize
    :: Header OddBlock -> SizeInBytes
  -- Here I realized we need to incorporate the block size in the header.
  nodeBlockFetchSize = oddBlockSize . oddBlockSignedPart

  nodeIsEBB = const Nothing

  nodeImmDbChunkInfo
    :: Proxy OddBlock
    -> TopLevelConfig OddBlock
    -> ChunkInfo
  nodeImmDbChunkInfo _ cfg = UniformChunkSize (ChunkSize False slots)
    where
      slots = slotsPerEpoch $ configLedger cfg

  -- The node start time determines when the __network__ started. This is used
  -- in combination with the blockchain time and slot duration to determine the
  -- current slot.
  nodeStartTime _ cfg = SystemStart $ cfgNodeStartTime $ configLedger cfg

  -- Here I realized I should be able to extract a 'NetworkMagic' from the
  -- configuration. For this simple example we won't be running different
  -- networks, so we can simply return a constant. However, even though the node
  -- config is unaware of the 'RunNode', it seems that this typeclass
  -- constraints quite heavily what run node should contain ...
  nodeNetworkMagic _ _ = NetworkMagic 0

  nodeProtocolMagicId _ _ = ProtocolMagicId 0

  nodeHashInfo :: Proxy OddBlock -> HashInfo (Hash (Header OddBlock))
  --
  -- This means we need to return:
  --
  -- > data HashInfo hash = HashInfo
  -- > { hashSize :: !Word32
  -- >  -- ^ A fixed size
  -- > , getHash  :: !(Get hash)
  -- > , putHash  :: !(hash -> Put)
  -- > }
  --
  --
  nodeHashInfo _ =
    HashInfo
    -- NOTE: for some strange reason the 'byteCount' is supposed to return a
    -- Natural, instead of a 'Word'. I don't think we'd need more than 2^32
    -- bytes for encoding hashes, since that's about 4GB.
    { hashSize = hashSize'
    , getHash =
        Crypto.Hash.UnsafeHash <$> Get.getByteString (fromIntegral hashSize')
    , putHash = \h -> Put.putByteString (Crypto.Hash.getHash h)
    }
    where
      hashSize' = fromIntegral
                $ Cardano.Crypto.Hash.Class.byteCount (Proxy :: Proxy ShortHash)
  -- See:
  --
  --
  -- > byronHashInfo :: HashInfo ByronHash
  -- > byronHashInfo = HashInfo { hashSize, getHash, putHash }
  -- >   where
  -- >     hashSize :: Word32
  -- >     hashSize = fromIntegral $ CC.hashDigestSize' @Crypto.Blake2b_256
  -- >
  -- >     getHash :: Get ByronHash
  -- >     getHash = do
  -- >       bytes <- Get.getByteString (fromIntegral hashSize)
  -- >       case Crypto.digestFromByteString bytes of
  -- >         Nothing     -> fail "digestFromByteString failed"
  -- >         Just digest -> return $ ByronHash $ CC.AbstractHash digest
  -- >
  -- >     putHash :: ByronHash -> Put
  -- >     putHash (ByronHash (CC.AbstractHash digest)) =
  -- >       Put.putByteString $ ByteArray.convert digest
  -- >
  --
  -- in @module Ouroboros.Consensus.Ledger.Byron.Block@

  -- We don't have a variable block size for now. We use 1M blocks
  nodeMaxBlockSize  = const $ 2^(20 :: Int)

  -- We set this to more than twice Byron's overhead. Just to be on the safe
  -- side.
  nodeBlockEncodingOverhead = const 2048

  nodeCheckIntegrity
    :: TopLevelConfig OddBlock
    -> OddBlock
    -> Bool
  -- Here I realized that to check the signature I need to have the issuer's
  -- vkey in the header.
  nodeCheckIntegrity _ blk
    =  -- Payload hash matches
       oddBlockPayloadHash signedPart == hash (oddBlockPayload blk)
       -- Block signature is correct
    && isRight (verifyDSIGN () vkey signedPart (oddBlockSignature hdr))
    where
      signedPart = oddBlockSignedPart hdr
      hdr        = oddBlockHeader blk
      vkey       = oddBlockIssuer signedPart

  ------------------------------------------------------------------------------
  -- Encoders
  ------------------------------------------------------------------------------

  -- From @edsko: the immutable database stores blocks as binary blobs when we
  -- are streaming them to clients we want to avoid deserializating those
  -- blocks. Only to serialize them again when we put them on the wire and so we
  -- just stream directly from disk. For the blocks that is easy, but we also
  -- want to stream headers in the same way and so ni order to do that, we need
  -- to know, given a block, what we need to "cut out "of that block to send
  -- over.
  nodeEncodeBlockWithInfo
    :: TopLevelConfig OddBlock
    -> OddBlock
    -> BinaryInfo Encoding
  -- QUESTION: are there functions to help getting this right?
  nodeEncodeBlockWithInfo _ blk =
    BinaryInfo
    { binaryBlob   = toCBOR blk
    , headerOffset = 1 -- CBOR tag
      -- NOTE: here we should check/assert that the length fits in a 'Word16'.
    , headerSize   = fromIntegral $ Strict.length $ serialize' $ getHeader blk
    }

  nodeEncodeHeader _ _ = toCBOR

  nodeEncodeWrappedHeader _ _ = encode

  nodeEncodeGenTx = toCBOR

  nodeEncodeGenTxId = toCBOR

  nodeEncodeHeaderHash _ = toCBOR

  nodeEncodeLedgerState _ = toCBOR

  nodeEncodeConsensusState _ _ = toCBOR

  nodeEncodeApplyTxError _ = toCBOR

  nodeEncodeTipInfo _ = toCBOR

  nodeEncodeQuery = \case {}

  nodeEncodeResult = \case {}

  ------------------------------------------------------------------------------
  -- Decoders
  ------------------------------------------------------------------------------

  nodeDecodeHeader _ _ = const <$> fromCBOR

  -- This is actually doing CBOR-in-CBOR for you. Reason for doing that: binary
  -- streaming of headers, i.e., server headers to nodes without deserialising
  -- and then reserialising them
  nodeDecodeWrappedHeader   = \_ _ -> decode

  nodeDecodeBlock _ = const <$> fromCBOR

  nodeDecodeGenTx = fromCBOR

  nodeDecodeGenTxId = fromCBOR

  nodeDecodeHeaderHash _ = fromCBOR

  nodeDecodeLedgerState _ = fromCBOR

  nodeDecodeConsensusState _ _ = fromCBOR

  nodeDecodeApplyTxError _ = fromCBOR

  nodeDecodeTipInfo _ = fromCBOR

  nodeDecodeQuery = error "OddChain.nodeDecodeQuery"

  nodeDecodeResult = \case {}


mkSignedPart
  :: VerKeyDSIGN MockDSIGN
  -> ChainHash OddBlock
  -> BlockNo
  -> SlotNo
  -> [Tx]
  -> SignedPart
mkSignedPart nodeVKey prevHash aBlockNo slotNo payload
  = SignedPart
    { oddBlockIssuer      = nodeVKey
    , oddBlockPrevHash    = prevHash
    , oddBlockNo          = aBlockNo
    , oddBlockSlot        = slotNo
    , oddBlockPayloadHash = hash payload
      -- We give an upper bound estimate of the block size
      --
      -- NOTE: encoding and computing the number of bytes would
      -- be a less error prone way of doing this. For the
      -- purposes of our prototype this method should suffice.
    , oddBlockSize        = tag 32 -- Prev hash
                          + tag 4  -- Block number
                          + tag 4  -- Slot number
                          + tag 4  -- Payload hash
                          + tag 4  -- Odd Block size
                          + tag (32 + 64) -- Signature
                          + tag (tag 4 * fromIntegral (length payload))
    }
  where
    tag x      = 1 + x -- Account for the CBOR tag when computing the upper
                       -- bound on the block size.

instance Condense OddBlock where
  condense = show

--------------------------------------------------------------------------------
-- Auxiliary  to run a node
--------------------------------------------------------------------------------

mkBftTopLevelConfig
  :: SecurityParam
  -> CoreNodeId
  -> NumCoreNodes
  -> Word64
  -> SlotLengths
  -> UTCTime
  -> TopLevelConfig OddBlock
mkBftTopLevelConfig k nodeId numCoreNodes numSlotsPerEpoch slotLengths startTime
  = TopLevelConfig
    { configConsensus =
        BftConfig
        { bftParams   =
            BftParams
            { bftNumNodes      = numCoreNodes
            , bftSecurityParam = k
            }
        , bftNodeId   = CoreId nodeId
        , bftSignKey  = signKey nodeId
        , bftVerKeys  = Map.fromList [
            (CoreId n, verKey n)
            | n <- enumCoreNodes numCoreNodes
            ]
        }
    , configLedger =
        OddConfig
        { slotsPerEpoch    = numSlotsPerEpoch
        , cfgNodeStartTime = startTime
        }
    , configBlock  = OddBlockConfig slotLengths
    }

signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
signKey (CoreNodeId n) = SignKeyMockDSIGN (fromIntegral n)

verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
verKey (CoreNodeId n) = VerKeyMockDSIGN (fromIntegral n)

mkExtLedgerState :: ExtLedgerState OddBlock
mkExtLedgerState =
  ExtLedgerState
  { ledgerState = initLedgerState
  , headerState = genesisHeaderState ()
  }

initLedgerState :: LedgerState OddBlock
initLedgerState
  = LedgerState
    { stLastApplied = GenesisPoint
    , phase = Increase 0
    }

--------------------------------------------------------------------------------
-- ToCBOR instances
--------------------------------------------------------------------------------

instance ToCBOR (Header OddBlock) where
  toCBOR (OddHeader
            oddBlockSignedPart
            oddBlockSignature
         )
    = encodeListLen 2
    <> toCBOR oddBlockSignedPart
    <> toCBOR oddBlockSignature

instance ToCBOR SignedPart where
  toCBOR (SignedPart
            oddBlockIssuer
            oddBlockPrevHash
            oddBlockNo
            oddBlockSlot
            oddBlockPayloadHash
            oddBlockSize
         )
    = encodeListLen 6
    <> toCBOR oddBlockIssuer
    <> toCBOR oddBlockPrevHash
    <> toCBOR oddBlockNo
    <> toCBOR oddBlockSlot
    <> toCBOR oddBlockPayloadHash
    <> toCBOR oddBlockSize

instance ToCBOR (ChainHash OddBlock) where
  toCBOR GenesisHash   = encodeListLen 0
  toCBOR (BlockHash h) = encodeListLen 1 <> toCBOR h

instance ToCBOR OddBlock where
  toCBOR (OddBlock aHeader aPayload)
    =  encodeListLen 2
    <> toCBOR aHeader
    <> toCBOR aPayload

instance ToCBOR (LedgerState OddBlock) where
  toCBOR (LedgerState aPoint aPhase)
    =  encodeListLen 2
    <> toCBOR aPoint
    <> toCBOR aPhase

instance ToCBOR Phase where
  toCBOR (Decrease i) = encodeListLen 2 <> encodeWord 0 <> toCBOR i
  toCBOR (Increase i) = encodeListLen 2 <> encodeWord 1 <> toCBOR i

instance ToCBOR OddTxError where
  toCBOR (NotOdd mobe i p) =
    encodeListLen 4 <> encodeWord 0 <> toCBOR mobe <> toCBOR i <> toCBOR p
  toCBOR (OddBut obe i p) =
    encodeListLen 4 <> encodeWord 1 <> toCBOR obe <> toCBOR i <> toCBOR p

instance ToCBOR OutOfBoundError where
  toCBOR = encodeInt . fromEnum

instance ToCBOR (Point OddBlock) where
  toCBOR = encodePoint toCBOR

--------------------------------------------------------------------------------
-- FromCBOR instances
--------------------------------------------------------------------------------

instance FromCBOR (Header OddBlock) where
  fromCBOR = do
    D.decodeListLenOf 2
    !theSignedPart <- fromCBOR
    !theSignature  <- fromCBOR
    pure $! OddHeader
            { oddBlockSignedPart = theSignedPart
            , oddBlockSignature = theSignature
            }

instance FromCBOR SignedPart where
  fromCBOR = do
    D.decodeListLenOf 6
    !theIssuer      <- fromCBOR
    !thePrevHash    <- fromCBOR
    !theBlockNo     <- fromCBOR
    !theBlockSlot   <- fromCBOR
    !thePayloadHash <- fromCBOR
    !theBlockSize   <- fromCBOR
    pure $! SignedPart
            { oddBlockIssuer      = theIssuer
            , oddBlockPrevHash    = thePrevHash
            , oddBlockNo          = theBlockNo
            , oddBlockSlot        = theBlockSlot
            , oddBlockPayloadHash = thePayloadHash
            , oddBlockSize        = theBlockSize
            }

instance FromCBOR (ChainHash OddBlock) where
  fromCBOR = do
    n <- D.decodeListLen
    case n of
      0 -> pure $! GenesisHash
      1 -> do
        !h <- fromCBOR
        pure $! BlockHash h
      _ -> cborError $ DecoderErrorUnknownTag "ChainHash" (fromIntegral n)

instance FromCBOR OddBlock where
  fromCBOR = do
    D.decodeListLenOf 2
    !theHeader  <- fromCBOR
    !thePayload <- fromCBOR
    pure $! OddBlock
            { oddBlockHeader  = theHeader
            , oddBlockPayload = thePayload
            }

instance FromCBOR (LedgerState OddBlock) where
  fromCBOR = do
    D.decodeListLenOf 2
    !aPoint <- fromCBOR
    !aPhase <- fromCBOR
    pure $! LedgerState
            { stLastApplied = aPoint
            , phase         = aPhase
            }

instance FromCBOR Phase where
  fromCBOR = do
    D.decodeListLenOf 2
    t <- D.decodeWord
    case t of
      0 -> do
        !i <- fromCBOR
        pure $! Decrease i
      1 -> do
        !i <- fromCBOR
        pure $! Increase i
      _ -> cborError $ DecoderErrorUnknownTag "Phase" (fromIntegral t)



instance FromCBOR OddTxError where
  fromCBOR = do
    D.decodeListLenOf 4
    t <- D.decodeWord
    case t of
      0 -> do
        !mobe <- fromCBOR
        !i    <- fromCBOR
        !p   <- fromCBOR
        pure $! NotOdd mobe i p
      1 -> do
        !obe <- fromCBOR
        !i   <- fromCBOR
        !p   <- fromCBOR
        pure $! OddBut obe i p
      _ -> cborError $ DecoderErrorUnknownTag "OddTxError" (fromIntegral t)

instance FromCBOR OutOfBoundError where
  fromCBOR = do
    !i <- D.decodeInt
    pure $! toEnum i

instance FromCBOR (Point OddBlock) where
  fromCBOR = decodePoint fromCBOR
