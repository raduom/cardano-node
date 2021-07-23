{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Trace.Forward.Network.Acceptor
  ( listenToForwarder
  -- | Export this function for Mux purpose.
  , acceptTraceObjects
  , acceptTraceObjectsInit
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           Control.Concurrent.Async (async, wait, waitAnyCancel)
import           Control.Monad (void)
import qualified Data.ByteString.Lazy as LBS
import           Data.Functor ((<&>))
import           Data.IORef (readIORef)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), MuxPeer (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits, runPeerWithLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, newNetworkMutableState,
                                           nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion,
                                                               simpleSingletonVersions)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import qualified Trace.Forward.Protocol.Acceptor as Acceptor
import qualified Trace.Forward.Protocol.Codec as Acceptor
import           Trace.Forward.Protocol.Limits (byteLimitsTraceForward, timeLimitsTraceForward)
import           Trace.Forward.Protocol.Type
import           Trace.Forward.Queue (getTraceObjects)
import           Trace.Forward.Configuration (AcceptorConfiguration (..), HowToConnect (..))

listenToForwarder
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  =>  IOManager
  -> AcceptorConfiguration lo
  -> ([lo] -> IO ())
  -> (NodeInfo -> IO ())
  -> IO ()
listenToForwarder iomgr config@AcceptorConfiguration{forwarderEndpoint} loHandler niHandler =
  case forwarderEndpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iomgr localPipe
          address = localAddressFromPath localPipe
      doListenToForwarder snocket address noTimeLimitsHandshake app
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let snocket = socketSnocket iomgr
          address = Socket.addrAddress listenAddress
      doListenToForwarder snocket address timeLimitsHandshake app
 where
  app =
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
      [ MiniProtocol
          { miniProtocolNum    = MiniProtocolNum 1
          , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
          , miniProtocolRun    = acceptTraceObjects config loHandler niHandler
          }
      ]

doListenToForwarder
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  -> IO ()
doListenToForwarder snocket address timeLimits app = do
  networkState <- newNetworkMutableState
  nsAsync <- async $ cleanNetworkMutableState networkState
  clAsync <- async . void $
    withServerNode
      snocket
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      address
      unversionedHandshakeCodec
      timeLimits
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      acceptableVersion
      (simpleSingletonVersions
        UnversionedProtocol
        UnversionedProtocolData
        (SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync -> wait serverAsync -- Block until async exception.
  void $ waitAnyCancel [nsAsync, clAsync]

acceptTraceObjects
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> ([lo] -> IO ())
  -> (NodeInfo -> IO ())
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
acceptTraceObjects config loHandler niHandler =
  ResponderProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeerWithLimits
        (acceptorTracer config)
        (Acceptor.codecTraceForward CBOR.encode CBOR.decode
                                    CBOR.encode CBOR.decode
                                    CBOR.encode CBOR.decode)
        (byteLimitsTraceForward (fromIntegral . LBS.length))
        timeLimitsTraceForward
        channel
        (Acceptor.traceAcceptorPeer $
          acceptorActions config loHandler niHandler True False)

acceptTraceObjectsInit
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo
  -> ([lo] -> IO ())
  -> (NodeInfo -> IO ())
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
acceptTraceObjectsInit config loHandler niHandler =
  InitiatorProtocolOnly $
    MuxPeerRaw $ \channel ->
      runPeerWithLimits
        (acceptorTracer config)
        (Acceptor.codecTraceForward CBOR.encode CBOR.decode
                                    CBOR.encode CBOR.decode
                                    CBOR.encode CBOR.decode)
        (byteLimitsTraceForward (fromIntegral . LBS.length))
        timeLimitsTraceForward
        channel
        (Acceptor.traceAcceptorPeer $
          acceptorActions config loHandler niHandler True False)

acceptorActions
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => AcceptorConfiguration lo -- ^ Acceptor's configuration.
  -> ([lo] -> IO ())          -- ^ The handler for accepted 'TraceObject's.
  -> (NodeInfo -> IO ())      -- ^ The handler for accepted info about the node.
  -> Bool                     -- ^ The flag for node's info request: only once in the beginning.
  -> Bool                     -- ^ The flag for end of session: if 'True', 'SendMsgDone' will be sent.
  -> Acceptor.TraceAcceptor lo IO ()
acceptorActions config@AcceptorConfiguration{whatToRequest, shouldWeStop} loHandler niHandler askForNI False =
  -- We are able to send request for:
  -- 1. node's info,
  -- 2. new 'TraceObject's.
  -- But request for node's info should be sent only once (in the beginning of session).
  if askForNI
    then
      Acceptor.SendMsgNodeInfoRequest $ \replyWithNI -> do
        niHandler replyWithNI
        readIORef shouldWeStop <&> acceptorActions config loHandler niHandler False
    else
      Acceptor.SendMsgTraceObjectsRequest TokBlocking whatToRequest $ \replyWithTraceObjects -> do
        loHandler $ getTraceObjects replyWithTraceObjects
        readIORef shouldWeStop <&> acceptorActions config loHandler niHandler False

acceptorActions _ _ _ _ True =
  Acceptor.SendMsgDone $ return ()
