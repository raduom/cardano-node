module Trace.Forward.Configuration
  ( AcceptorConfiguration (..)
  , ForwarderConfiguration (..)
  , HowToConnect (..)
  , Host
  , Port
  ) where

import           Control.Tracer (Tracer)
import           Data.IORef (IORef)
import           Data.Text (Text)
import           Data.Word (Word16)
import           Ouroboros.Network.Driver (TraceSendRecv)

import           Trace.Forward.Protocol.Type

type Host = Text
type Port = Word16

-- | Specifies how to connect to the peer.
data HowToConnect
  = LocalPipe    !FilePath    -- ^ Local pipe (UNIX or Windows).
  | RemoteSocket !Host !Port  -- ^ Remote socket (host and port).
  deriving Show

-- | Acceptor configuration, parameterized by trace item's type.
data AcceptorConfiguration lo = AcceptorConfiguration
  { -- | The tracer that will be used by the acceptor in its network layer.
    acceptorTracer    :: !(Tracer IO (TraceSendRecv (TraceForward lo)))
    -- | The endpoint that will be used to listen to the forwarder.
  , forwarderEndpoint :: !HowToConnect
    -- | The request specifies how many 'TraceObject's will be requested.
  , whatToRequest     :: !NumberOfTraceObjects
    -- | 'IORef' that can be used as a brake: if an external thread sets
    --   it to 'True', the acceptor will send 'MsgDone' message to the
    --   forwarder and their session will be closed.
  , shouldWeStop      :: !(IORef Bool)
  }

-- | Forwarder configuration, parameterized by trace item's type.
data ForwarderConfiguration lo = ForwarderConfiguration
  { -- | The tracer that will be used by the forwarder in its network layer.
    forwarderTracer    :: !(Tracer IO (TraceSendRecv (TraceForward lo)))
    -- | The endpoint that will be used to connect to the acceptor.
  , acceptorEndpoint   :: !HowToConnect
    -- | An action that returns node's information.
  , getNodeInfo        :: !(IO NodeInfo)
  }
