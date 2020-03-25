{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module OddWallet where

import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import           Control.Tracer (Tracer (Tracer), nullTracer)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy (Proxy))
import           Data.Void (Void)
import           Data.Word (Word64)
import           Text.Pretty.Simple (pPrint)
import           GHC.Generics (Generic)

import           Crypto.Number.Generate (generateBetween)

import           Cardano.Binary (fromCBOR, toCBOR)

import           Network.Mux.Types (AppType (InitiatorApp))
import           Network.TypedProtocol.Core (Peer, PeerRole (AsClient))
import           Ouroboros.Consensus.Mempool.API (ApplyTxErr)
import           Ouroboros.Consensus.Node (nodeDecodeApplyTxError,
                     nodeDecodeGenTx, nodeEncodeApplyTxError, nodeEncodeGenTx)
import           Ouroboros.Network.Block (Point, Tip)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Codec (Codec, DeserialiseFailure)
import           Ouroboros.Network.Driver.Simple (TraceSendRecv, runPeer)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux (RunMiniProtocol (InitiatorProtocolOnly))
import           Ouroboros.Network.Mux (MuxPeer (MuxPeer, MuxPeerRaw),
                     OuroborosApplication)
import           Ouroboros.Network.NodeToClient
                     (NodeToClientProtocols (NodeToClientProtocols),
                     NodeToClientVersion (NodeToClientV_1),
                     NodeToClientVersionData (NodeToClientVersionData),
                     connectTo, localChainSyncProtocol,
                     localTxSubmissionProtocol, networkMagic,
                     nodeToClientCodecCBORTerm, nodeToClientProtocols)
import           Ouroboros.Network.Protocol.ChainSync.Client
                     (ChainSyncClient (ChainSyncClient),
                     ClientStIdle (SendMsgFindIntersect, SendMsgRequestNext),
                     ClientStIntersect (ClientStIntersect, recvMsgIntersectFound, recvMsgIntersectNotFound),
                     ClientStNext (ClientStNext, recvMsgRollBackward, recvMsgRollForward),
                     chainSyncClientPeer)
import           Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import           Ouroboros.Network.Protocol.Handshake.Version (DictVersion,
                     Versions)
import           Ouroboros.Network.Protocol.Handshake.Version
                     (DictVersion (DictVersion), simpleSingletonVersions)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
                     (LocalTxClientStIdle,
                     LocalTxClientStIdle (SendMsgSubmitTx),
                     LocalTxSubmissionClient (LocalTxSubmissionClient),
                     localTxSubmissionClientPeer)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
                     (codecLocalTxSubmission)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type
                     (LocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as TxSubmission
import           Ouroboros.Network.Snocket (socketSnocket)
import           Ouroboros.Network.Snocket (LocalAddress)
import           Ouroboros.Network.Socket (ConnectionId,
                     NetworkConnectTracers (NetworkConnectTracers),
                     nctHandshakeTracer, nctMuxTracer)

import           OddChain
import           OddNode hiding (nodeId, enableTracers)


data WalletConfig =
  WalletConfig
  { nodeId        :: Word64
    -- ^ Node to connect to.
  , enabledTracers :: [WalletTracer]
  } deriving (Eq, Show, Generic)

data WalletTracer
  = ChainSync
  | LocalTxSubmission
  | Rolls
  -- ^ Log roll forward and backward messages.
  | All
  -- ^ Enable all tracers.
  deriving (Eq, Show, Generic, Read)

shouldTraceChainSync :: WalletConfig -> Bool
shouldTraceChainSync WalletConfig { enabledTracers } =
  ChainSync `elem` enabledTracers || All `elem` enabledTracers

shouldTraceLocalTxSubmissions :: WalletConfig -> Bool
shouldTraceLocalTxSubmissions WalletConfig { enabledTracers } =
  LocalTxSubmission `elem` enabledTracers || All `elem` enabledTracers

shouldTraceRolls :: WalletConfig -> Bool
shouldTraceRolls WalletConfig { enabledTracers } =
  Rolls `elem` enabledTracers || All `elem` enabledTracers

runWallet :: WalletConfig -> IO ()
runWallet cfg@WalletConfig{ nodeId } = withIOManager $ \iocp -> do
  connectTo
    (socketSnocket iocp)
    NetworkConnectTracers
      { nctMuxTracer       = nullTracer
      , nctHandshakeTracer = nullTracer
      }
    (localInitiatorNetworkApplication cfg)
    (mkLocalSocketPath nodeId)

localInitiatorNetworkApplication
  :: WalletConfig
  -> Versions NodeToClientVersion
              DictVersion
              (   ConnectionId LocalAddress
               -> OuroborosApplication 'InitiatorApp Lazy.ByteString IO Void Void
              )
localInitiatorNetworkApplication cfg =
  simpleSingletonVersions
    NodeToClientV_1
    (NodeToClientVersionData { networkMagic = mkNodeNetworkMagic })
    (DictVersion nodeToClientCodecCBORTerm)
    (ouroborosApp cfg)

chainSyncTracer
  :: WalletConfig -> Tracer IO  (TraceSendRecv (ChainSync OddBlock (Tip OddBlock)))
chainSyncTracer cfg =
  if shouldTraceChainSync cfg
  then Tracer pPrint
  else nullTracer

localTxSubmissionTracer
  :: WalletConfig -> Tracer IO (TraceSendRecv (LocalTxSubmission (GenTx OddBlock) OddTxError))
localTxSubmissionTracer cfg=
  if shouldTraceLocalTxSubmissions cfg
  then Tracer pPrint
  else nullTracer

ouroborosApp
  :: WalletConfig
  -> ConnectionId LocalAddress
  -> OuroborosApplication 'InitiatorApp Lazy.ByteString IO Void Void
ouroborosApp cfg _connectionId =
  -- It is important to pass the parameters in the right order. The types of
  -- both parameters are the same, so there's nothing that prevent us from
  -- inadvertently using them in the wrong order.
  --
  -- So if you get errors like
  --
  -- > Mux ConnectionId  ... Miniprotocol MiniProtocolNum 6 ModeResponder
  -- >    terminated with exception DeserialiseFailure 2 "codecLocalTxSubmission.Idle: unexpected key"
  --
  -- Look at the order of the parameters passed to 'nodeToClientProtocols'.
  nodeToClientProtocols
    NodeToClientProtocols
    { localChainSyncProtocol    = chainSyncProtocol cfg
    , localTxSubmissionProtocol = txSubmissionProtocol cfg
    }

txSubmissionProtocol :: WalletConfig -> RunMiniProtocol 'InitiatorApp Lazy.ByteString IO Void Void
txSubmissionProtocol cfg
  = InitiatorProtocolOnly
  $ MuxPeerRaw
  $ \channel -> do
           runPeer
             (localTxSubmissionTracer cfg)
             localTxSubmissionCodec
             channel
             localTxSubmissionPeer

chainSyncProtocol
  :: WalletConfig
  -> RunMiniProtocol 'InitiatorApp Lazy.ByteString IO Void Void
chainSyncProtocol cfg
  = InitiatorProtocolOnly
  $ MuxPeer
      (chainSyncTracer cfg)
      localChainSyncCodec
      (chainSyncPeer cfg)

localTxSubmissionCodec
  :: Codec (LocalTxSubmission (GenTx OddBlock) (ApplyTxErr OddBlock))
            DeserialiseFailure IO Lazy.ByteString
localTxSubmissionCodec =
  codecLocalTxSubmission
    nodeEncodeGenTx
    nodeDecodeGenTx
    (nodeEncodeApplyTxError (Proxy @OddBlock))
    (nodeDecodeApplyTxError (Proxy @OddBlock))

localChainSyncCodec
  :: Codec (ChainSync OddBlock (Tip OddBlock))
           DeserialiseFailure IO Lazy.ByteString
localChainSyncCodec =
  codecChainSync
    (Block.wrapCBORinCBOR toCBOR)
    (Block.unwrapCBORinCBOR (fmap const fromCBOR))
    (Block.encodePoint toCBOR)
    (Block.decodePoint fromCBOR)
    (Block.encodeTip toCBOR)
    (Block.decodeTip fromCBOR)

localTxSubmissionPeer
  :: Peer
       (LocalTxSubmission (GenTx OddBlock) OddTxError)
       'AsClient -- This is the peer role
       'TxSubmission.StIdle   -- Expected peer state
       IO
       Void
localTxSubmissionPeer
  = localTxSubmissionClientPeer
  $ LocalTxSubmissionClient
  $ loop (Increase 0) -- We bootstrap the loop with some
  where
    loop
      :: Phase
      -> IO (LocalTxClientStIdle (GenTx OddBlock) OddTxError IO Void)
    loop phase = do
      threadDelay (10^(6 :: Int))
      -- For now we have a simple way of generating transactions.
      let
      tx <- fmap (OddTx . Tx . fromIntegral) $ generateAccordingTo phase
      pure $ SendMsgSubmitTx tx (txSubmisionResultHandler phase)
      where
        generateAccordingTo (Increase i) = fmap mkOdd $ generateBetween upper (upper + 10)
          where
            upper   = fromIntegral i + 1
            mkOdd j = if even j then j + 1 else j
        generateAccordingTo (Decrease i) = fmap mkOdd $ generateBetween (lower - 10) lower
          where
            lower   = fromIntegral $ i - 1
            mkOdd j = if even j then j - 1 else j

        txSubmisionResultHandler
          :: Phase
          -> Maybe OddTxError
          -> IO (LocalTxClientStIdle (GenTx OddBlock) OddTxError IO Void)
        txSubmisionResultHandler currentPhase  Nothing      =
          loop currentPhase
        txSubmisionResultHandler _currentPhase (Just txErr) = do
          loop (errorPhase txErr)

chainSyncPeer
  :: WalletConfig
  -> Peer
       (ChainSync OddBlock (Tip OddBlock))
       'AsClient
       'ChainSync.StIdle
       IO
       Void
chainSyncPeer cfg
  = chainSyncClientPeer
  $ ChainSyncClient
  $ pure
  $ -- We notify the core node about our latest points at which we're
    -- synchronized.
    --
    -- This simple wallet is stateless, so we always synchronize from the
    -- genesis block.
    SendMsgFindIntersect
      [Block.genesisPoint]
      ClientStIntersect
      {
        recvMsgIntersectFound =
          \point chainTip ->
            ChainSyncClient $ do
              when (shouldTraceChainSync cfg) $ do
                putStrLn "Intersect found:"
                pPrint   point
                pPrint   chainTip
              pure (clientStIdle cfg)
      , recvMsgIntersectNotFound =
         -- TODO: handle this properly. Not done in the demo.
          \chainTip -> ChainSyncClient $ error $ show chainTip
      }


-- Handler for the idle state
clientStIdle :: WalletConfig -> ClientStIdle OddBlock (Tip OddBlock) IO Void
clientStIdle cfg =
  SendMsgRequestNext (clientStNext cfg) (pure $ clientStNext cfg)

-- Handler for the next state
--
-- The node tells the client to roll forward or backward. The node knows
-- where the tip of the client is.
clientStNext :: WalletConfig -> ClientStNext OddBlock (Tip OddBlock) IO Void
clientStNext cfg =
  ClientStNext
  { recvMsgRollForward  = handleRollForward cfg
  , recvMsgRollBackward = handleRollBackward cfg
  }

handleRollForward
  :: WalletConfig
  -> OddBlock
  -- ^ New block we receive from the client
  -> Tip OddBlock
  -- ^ Current tip of the chain.
  --
  -- The current tip can be used for different purposes:
  --
  --   - if the current tip is the same as the one the client stores locally
  --     (assuming it does that), then we can avoid rolling forward and use our
  --     own chain.
  --   - it might be use to get an idea of the synchronization progress.
  --   - the client might decide it want to disconnect if the node is a large
  --     number of slots behind the client.
  -> ChainSyncClient OddBlock (Tip OddBlock) IO Void
handleRollForward cfg block chainTip
  = ChainSyncClient
  $ do
      -- For now we simply print what we got.
      when (shouldTraceRolls cfg) $ do
        putStrLn  "Rolling forward."
        pPrint     block
        pPrint     chainTip
      pure (clientStIdle cfg)

handleRollBackward
  :: WalletConfig
  -> Point OddBlock
  -> Tip OddBlock
  -> ChainSyncClient OddBlock (Tip OddBlock) IO Void
handleRollBackward cfg point chainTip
  = ChainSyncClient
  $ do
      -- we are requested to roll backward to point 'point', the core
      -- node's chain's tip is 'tip'.
      when (shouldTraceRolls cfg) $ do
        putStrLn "Rolling backward."
        pPrint   point
        pPrint   chainTip
      pure (clientStIdle cfg)
