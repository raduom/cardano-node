{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Network.Forwarder
  ( connectToAcceptor
  -- | Export this function for Mux purpose.
  , forwardTraceObjects
  , forwardTraceObjectsResp
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits, runPeerWithLimits)
import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (connectToNode, nullNetworkConnectTracers)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Configuration (ForwarderConfiguration (..), HowToConnect (..))
import           Trace.Forward.Queue (readItems)
import qualified Trace.Forward.Protocol.Forwarder as Forwarder
import qualified Trace.Forward.Protocol.Codec as Forwarder
import           Trace.Forward.Protocol.Limits (byteLimitsTraceForward, timeLimitsTraceForward)

connectToAcceptor
  :: (CBOR.Serialise lo,
      ShowProxy lo)
  => IOManager
  -> ForwarderConfiguration lo
  -> TBQueue lo
  -> IO ()
connectToAcceptor iomgr config@ForwarderConfiguration{acceptorEndpoint} loQueue =
  case acceptorEndpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iomgr localPipe
          address = localAddressFromPath localPipe
      doConnectToAcceptor snocket address noTimeLimitsHandshake app
    RemoteSocket host port -> do
      acceptorAddr:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let snocket = socketSnocket iomgr
          address = Socket.addrAddress acceptorAddr
      doConnectToAcceptor snocket address timeLimitsHandshake app
 where
  app =
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
      [ MiniProtocol
          { miniProtocolNum    = MiniProtocolNum 1
          , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
          , miniProtocolRun    = forwardTraceObjects config loQueue
          }
      ]

doConnectToAcceptor
  :: Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
  -> IO ()
doConnectToAcceptor snocket address timeLimits app =
  connectToNode
    snocket
    unversionedHandshakeCodec
    timeLimits
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (simpleSingletonVersions
       UnversionedProtocol
       UnversionedProtocolData
       app)
    Nothing
    address

forwardTraceObjects
  :: (CBOR.Serialise lo,
      ShowProxy lo)
  => ForwarderConfiguration lo
  -> TBQueue lo
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
forwardTraceObjects config loQueue =
  InitiatorProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeerWithLimits
        (forwarderTracer config)
        (Forwarder.codecTraceForward CBOR.encode CBOR.decode
                                     CBOR.encode CBOR.decode
                                     CBOR.encode CBOR.decode)
        (byteLimitsTraceForward (fromIntegral . LBS.length))
        timeLimitsTraceForward
        channel
        (Forwarder.traceForwarderPeer $ readItems config loQueue)

forwardTraceObjectsResp
  :: (CBOR.Serialise lo,
      ShowProxy lo)
  => ForwarderConfiguration lo
  -> TBQueue lo
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
forwardTraceObjectsResp config loQueue =
  ResponderProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeerWithLimits
        (forwarderTracer config)
        (Forwarder.codecTraceForward CBOR.encode CBOR.decode
                                     CBOR.encode CBOR.decode
                                     CBOR.encode CBOR.decode)
        (byteLimitsTraceForward (fromIntegral . LBS.length))
        timeLimitsTraceForward
        channel
        (Forwarder.traceForwarderPeer $ readItems config loQueue)
