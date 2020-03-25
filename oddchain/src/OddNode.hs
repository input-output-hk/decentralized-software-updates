{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module OddNode where

import           Codec.Serialise (DeserialiseFailure)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Network.Socket (AddrInfo,
                     AddrInfoFlag (AI_ADDRCONFIG, AI_PASSIVE), ServiceName,
                     SocketType (Stream), addrAddress, addrFlags,
                     addrSocketType, defaultHints, getAddrInfo)
import           Text.Pretty.Simple (pPrint)

import           Control.Tracer (Tracer (Tracer), nullTracer)

import           Ouroboros.Consensus.Ledger.Abstract (Query)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (ShowQuery)

import           Ouroboros.Consensus.BlockchainTime (SlotLengths)
import           Ouroboros.Consensus.BlockchainTime.SlotLength
                     (slotLengthFromSec)
import           Ouroboros.Consensus.BlockchainTime.SlotLengths
                     (singletonSlotLengths)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr)
import           Ouroboros.Consensus.Node (IsProducer (IsProducer), NodeArgs,
                     NodeKernel, ProtocolInfo, RemoteConnectionId, Tracers)
import qualified Ouroboros.Consensus.Node as Node
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes,
                     enumCoreNodes)
import           Ouroboros.Consensus.Node.ProtocolInfo
                     (ProtocolInfo (ProtocolInfo), pInfoConfig,
                     pInfoInitLedger, pInfoInitState)
import           Ouroboros.Consensus.Node.ProtocolInfo
                     (NumCoreNodes (NumCoreNodes))
import           Ouroboros.Consensus.Node.Tracers (Tracers' (Tracers),
                     blockFetchClientTracer, blockFetchDecisionTracer,
                     blockFetchServerTracer, blockchainTimeTracer,
                     chainSyncClientTracer, chainSyncServerBlockTracer,
                     chainSyncServerHeaderTracer, forgeTracer,
                     localTxSubmissionServerTracer, mempoolTracer, nullTracers,
                     txInboundTracer, txOutboundTracer)
import           Ouroboros.Consensus.NodeId (CoreNodeId (CoreNodeId))
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers,
                     ProtocolTracers' (ProtocolTracers), nullProtocolTracers,
                     ptBlockFetchSerialisedTracer, ptBlockFetchTracer,
                     ptChainSyncSerialisedTracer, ptChainSyncTracer,
                     ptLocalChainSyncTracer, ptLocalStateQueryTracer,
                     ptLocalTxSubmissionTracer, ptTxSubmissionTracer)
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam)
import           Ouroboros.Consensus.Protocol.Abstract
                     (SecurityParam (SecurityParam))
import           Ouroboros.Consensus.Storage.ChainDB (ChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Network.Block (HasHeader)
import           Ouroboros.Network.Diffusion
                     (DiffusionArguments (DiffusionArguments),
                     DiffusionTracers (DiffusionTracers), daAddresses,
                     daDnsProducers, daIpProducers, daLocalAddress,
                     dtDnsResolverTracer, dtDnsSubscriptionTracer,
                     dtErrorPolicyTracer, dtHandshakeLocalTracer,
                     dtHandshakeTracer, dtIpSubscriptionTracer,
                     dtLocalErrorPolicyTracer, dtMuxLocalTracer, dtMuxTracer)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Magic (NetworkMagic (NetworkMagic))
import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.Subscription.Ip
                     (IPSubscriptionTarget (IPSubscriptionTarget), ispIps,
                     ispValency)

import           OddChain


-- | Configuration for running a node.
--
--
data RunConfig =
  RunConfig
  { k                   :: !SecurityParam
    -- ^ Security parameter. This value is used by the consensus layer only.
  , numSlotsPerEpoch    :: !Word64
    -- ^ How many slots are in an epoch.
  , slotLengths         :: !SlotLengths
    -- ^ Number of seconds that a slot spans.
  , nodeId              :: !CoreNodeId
    -- ^ Id of the node. This must be between @0@ and @numCoreNodes - 1@. The
    -- node id is used by the BFT protocol to identify which node should be
    -- producing a block in a given slot.
  , numCoreNodes        :: !NumCoreNodes
    -- ^ Number of core nodes in the network. These are the nodes that can
    -- produce a block. Every node run via 'oddRun' is a core node.
  , nodeLocalSocketPath :: !FilePath
    -- ^ Path to the local socket that the node uses to communicate to local
    -- clients.
  , nodeNetworkMagic    :: !NetworkMagic
    -- ^ Network magic to distinguish among the different networks.
  , enableTracers       :: ![NodeTracer]
  , startTime           :: !UTCTime
    -- ^ Time at which the network was started. This is used to determine the
    -- current slot number. When testing or simulating the oddchain use a start
    -- time close the current time to avoid a large number of empty slots (and
    -- epochs). Failure to doing this will result in a database of a
    -- considerable size. Of course, its size depends of the slots per second,
    -- and number of slots per epoch.
  }
  deriving (Show)


data NodeTracer
  = Network
    -- ^ Enable most (but not all!) of the network tracers. We'll be
    -- experimenting with local nodes, at least in the beginning, so tracers
    -- like 'dtIpSubscriptionTracer', 'dtDnsSubscriptionTracer',
    -- 'dtDnsResolverTracer' won't be needed.
  | Consensus
  | Protocol
  | Database
  deriving (Show, Generic, Read, Eq)

-- | Entry point for running a node.
--
-- This function defines a very thin layer on top of
-- @Ouroboros.Consensus.Node.run@, just to illustrate how this can be done.
-- Therefore this function doesn't offer a great deal of flexibility:
--
-- - the database is placed in @/tmp/odd-db-nodeId@, making it Linux specific.
--
-- - it is not possible to configure remote peers. We assume all peers are
--   running in the same machine.
--
-- - port numbers are assigned starting from @3030@.
--
-- When forking off this demo you'd probably want to add more flexibility to the function
-- that interfaces with @Ouroboros.Consensus.Node.run@.
--
oddRun :: RunConfig -> IO ()
oddRun RunConfig { k
                 , numSlotsPerEpoch
                 , slotLengths
                 , nodeId
                 , numCoreNodes
                 , nodeLocalSocketPath
                 , nodeNetworkMagic
                 , enableTracers
                 , startTime
                 }
  = do
  let
    myServiceName     = filter (== nodePort nodeId) serviceNameList
    theirServiceNames = filter (/= nodePort nodeId) serviceNameList
  myDiffussionAddress <-
    fmap concat $ traverse mkDiffusionAddresses myServiceName
  theirDiffussionAddresses <-
    fmap concat $ traverse mkDiffusionAddresses theirServiceNames
  Node.run
    consensusTracers
    protocolTracers
    chainDBTracers
    diffusionTracers
    (diffusionArguments myDiffussionAddress theirDiffussionAddresses)
    nodeNetworkMagic
    databasePath
    protocolInfo
    IsProducer
    chainDBArgsCustomizer
    nodeArgsCustomizer
    nodeKernelCallback
  where
    -- In cardano-node these are defined in:
    --
    -- cardano-node/cardano-node/src/Cardano/Tracing/Tracers.hs#L419
    --
    -- Remember that tracers are monoids so you can use `<>`.
    consensusTracers :: Tracers IO RemoteConnectionId OddBlock
    consensusTracers =
      if Consensus `elem` enableTracers
      then prettyConsensusTracers
      else nullTracers

    protocolTracers :: ProtocolTracers IO
                                       RemoteConnectionId
                                       LocalConnectionId
                                       OddBlock
                                       DeserialiseFailure
    protocolTracers =
      if Protocol `elem` enableTracers
      then prettyProtocolTracers
      else nullProtocolTracers

    chainDBTracers :: Tracer IO (ChainDB.TraceEvent OddBlock)
    chainDBTracers =
      if Database `elem` enableTracers
      then Tracer traceDB
      else nullTracer
      where
        -- Here the events of interest can be filtered. See
        --
        -- > Ouroboros.Consensus.Storage.ChainDB.Impl.Types
        --
        traceDB event =
          case event of
            ChainDB.TraceAddBlockEvent addBlockEvent -> pPrint addBlockEvent
            _                                        -> pure ()

    diffusionTracers :: DiffusionTracers
    diffusionTracers =
      -- We're not subscribing anywhere, so the subscription tracers can be off.
      --
      -- Mux is quite verbose, so it can be turned on if needed.
      --
      -- Error policy tracers ('ErrorPolicy') are essential.
      --
      -- Handshake tracer sends only two or three messages and it is important
      -- since it will error if the protocol version disagrees.
      DiffusionTracers
      { dtIpSubscriptionTracer   = nullTracer
      , dtDnsSubscriptionTracer  = nullTracer
      , dtDnsResolverTracer      = nullTracer
      , dtMuxTracer              = mkTracer Network
      , dtMuxLocalTracer         = mkTracer Network
      , dtHandshakeTracer        = mkTracer Network
      -- Tracers below were also defined using the 'nullTracer' in
      -- @cardano-node@. See @Cardano.Node.Run.createDiffusionTracers@.
      , dtHandshakeLocalTracer   = mkTracer Network
      , dtErrorPolicyTracer      = mkTracer Network
      , dtLocalErrorPolicyTracer = mkTracer Network
      }

    diffusionArguments :: [AddrInfo] -> [AddrInfo] -> DiffusionArguments
    diffusionArguments myDiffussionAddresses theirDiffussionAddresses =
      DiffusionArguments
      { daAddresses    = myDiffussionAddresses
      , daLocalAddress = nodeLocalSocketPath
        -- Path to a local socket (or named pipe). This is the address that
        -- local clients will use to connect to the node. Here clients are
        -- processes, like a wallet, that don't talk to the node over a network
        -- connection, but over a socket.
      , daIpProducers  =
        IPSubscriptionTarget
        { ispIps = fmap addrAddress theirDiffussionAddresses
        , ispValency = length theirDiffussionAddresses
        }
        -- If you only use ip addresses (no domain names), you'd just want to
        -- use daIpProcuders , which is:
        --
        -- data IPSubscriptionTarget = IPSubscriptionTarget {
        -- >  -- | List of destinations to possibly connect to
        -- >   ispIps     :: ![Socket.SockAddr]
        -- >  -- | Number of parallel connections to keep actice.
        -- >  , ispValency :: !Int
        -- >  } deriving (Eq, Show)
        --
        -- so you just pass the addresses to which you'd like the node to
        -- connect plus a valency (the number of active connections, if you want
        -- all of them to be active pass the length of the ispIps list).
      , daDnsProducers = []
        -- These are the domain names that will resolve to IP addresses the
        -- relay nodes.
      }
    databasePath :: FilePath
    databasePath = "/tmp/odd-db-" ++ show i -- TODO: we might want to make sure this exists.
      where CoreNodeId i = nodeId

    protocolInfo :: ProtocolInfo OddBlock
    protocolInfo =
      ProtocolInfo
      { pInfoConfig     =
          mkBftTopLevelConfig k nodeId numCoreNodes numSlotsPerEpoch slotLengths startTime
      , pInfoInitState  = ()
      , pInfoInitLedger = mkExtLedgerState
      }

    chainDBArgsCustomizer
      :: ChainDbArgs IO OddBlock
      -> ChainDbArgs IO OddBlock
    chainDBArgsCustomizer = id

    nodeArgsCustomizer
      :: NodeArgs IO RemoteConnectionId OddBlock
      -> NodeArgs IO RemoteConnectionId OddBlock
    nodeArgsCustomizer = id

    nodeKernelCallback
      :: ResourceRegistry IO
      -> NodeKernel IO RemoteConnectionId OddBlock
      -> IO ()
    nodeKernelCallback _ _ = pure ()

    mkDiffusionAddresses :: ServiceName -> IO [AddrInfo]
    mkDiffusionAddresses serviceName =
      getAddrInfo (Just hints) (Just "localhost") (Just serviceName)
      where
        hints = defaultHints { addrFlags = [AI_PASSIVE, AI_ADDRCONFIG]
                             , addrSocketType = Stream
                             }

    serviceNameList :: [ServiceName]
    serviceNameList = fmap nodePort $ enumCoreNodes numCoreNodes

    nodePort :: CoreNodeId -> ServiceName
    nodePort (CoreNodeId i) = show $ 3030 + i

    mkTracer :: Show a => NodeTracer -> Tracer IO a
    mkTracer enable =
      if enable `elem` enableTracers
      then Tracer pPrint
      else nullTracer

prettyConsensusTracers
  :: ( MonadIO m
     , Show blk
     , Show (GenTx blk)
     , LedgerSupportsProtocol blk
     )
  => Tracers' peer blk (Tracer m)
prettyConsensusTracers
  -- We disable most of the tracers. Edit according to your needs.
  = Tracers
  { chainSyncClientTracer         = nullTracer
  , chainSyncServerHeaderTracer   = nullTracer
  , chainSyncServerBlockTracer    = nullTracer
  , blockFetchDecisionTracer      = nullTracer
  , blockFetchClientTracer        = nullTracer
  , blockFetchServerTracer        = nullTracer
  , txInboundTracer               = nullTracer
  , txOutboundTracer              = nullTracer
  , localTxSubmissionServerTracer = nullTracer
  , mempoolTracer                 = nullTracer
  , forgeTracer                   = Tracer pPrint
  , blockchainTimeTracer          = nullTracer
  }

prettyProtocolTracers
  :: ( MonadIO m
     , Show blk
     , Show peer
     , Show localPeer
     , Show (Header blk)
     , Show (GenTx blk)
     , Show (TxId (GenTx blk))
     , Show (ApplyTxErr blk)
     , ShowQuery (Query blk)
     , HasHeader blk
     )
  => ProtocolTracers m peer localPeer blk failure
prettyProtocolTracers =
  ProtocolTracers
  { ptChainSyncTracer            = Tracer pPrint
  , ptChainSyncSerialisedTracer  = Tracer pPrint
  , ptBlockFetchTracer           = Tracer pPrint
  , ptBlockFetchSerialisedTracer = Tracer pPrint
  , ptTxSubmissionTracer         = Tracer pPrint
  , ptLocalChainSyncTracer       = Tracer pPrint
  , ptLocalTxSubmissionTracer    = Tracer pPrint
  , ptLocalStateQueryTracer      = Tracer pPrint
  }

--------------------------------------------------------------------------------
-- Node configuration utilities
--------------------------------------------------------------------------------

-- | Platform-dependent path. Just for demo purposes.
mkLocalSocketPath :: Word64 -> String
mkLocalSocketPath aNodeId = "/tmp/oddsocket-" ++ show aNodeId

-- | Constant value. Just for demo purposes.
mkNodeNetworkMagic :: NetworkMagic
mkNodeNetworkMagic = NetworkMagic 15

mkRunConfig :: Word64 -> Word64 -> [NodeTracer] -> UTCTime -> RunConfig
mkRunConfig nodeCount aNodeId enableTracers aStartTime =
  RunConfig
  { k                     = SecurityParam 2
  , numSlotsPerEpoch      = 30 -- If the starting time of the network is far in
                               -- the past use a large number here to avoid
                               -- having a huge number of epoch files. For
                               -- testing and demo purposes we change epochs
                               -- every 5 minutes.
  , slotLengths           = singletonSlotLengths $ slotLengthFromSec 10
  , OddNode.nodeId        = CoreNodeId aNodeId
  , numCoreNodes          = NumCoreNodes nodeCount
  , nodeLocalSocketPath   = mkLocalSocketPath aNodeId
  , nodeNetworkMagic      = mkNodeNetworkMagic
  , OddNode.enableTracers = enableTracers
  , startTime             = aStartTime
  }
