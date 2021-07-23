module Test.Trace.Forward.Demo.Configs
  ( mkAcceptorConfig
  , mkForwarderConfig
  ) where

import           Control.Tracer (nullTracer)
import           Data.IORef (IORef)

import           Trace.Forward.Configuration
import           Trace.Forward.Protocol.Type

import           Test.Trace.Forward.Protocol.TraceItem

mkAcceptorConfig
  :: HowToConnect
  -> IORef Bool
  -> AcceptorConfiguration TraceItem
mkAcceptorConfig ep weAreDone = AcceptorConfiguration
  { acceptorTracer    = nullTracer
  , forwarderEndpoint = ep
  , whatToRequest     = NumberOfTraceObjects 10
  , shouldWeStop      = weAreDone
  }

mkForwarderConfig
  :: HowToConnect
  -> IO NodeInfo
  -> ForwarderConfiguration TraceItem
mkForwarderConfig ep getNI = ForwarderConfiguration
  { forwarderTracer  = nullTracer
  , acceptorEndpoint = ep
  , getNodeInfo      = getNI
  }
