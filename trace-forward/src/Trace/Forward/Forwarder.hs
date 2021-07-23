{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- This top-level module will be used by the forwarder application.
-- Forwarder application collects 'TraceObject's and sends them to
-- the acceptor application.
module Trace.Forward.Forwarder
  ( runTraceForwarder
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Exception (SomeAsyncException (..), fromException, tryJust)
import           Control.Tracer (showTracing, stdoutTracer, traceWith)
import           Data.Typeable (Typeable)
import           System.Time.Extra (sleep)

import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Configuration (ForwarderConfiguration (..))
import           Trace.Forward.Network.Forwarder (connectToAcceptor)

runTraceForwarder
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => IOManager                 -- ^ 'IOManager' from the external application.
  -> ForwarderConfiguration lo -- ^ Forwarder configuration.
  -> TBQueue lo                -- ^ The queue from which the forwarder will take 'TraceObject's.
  -> IO ()
runTraceForwarder iomgr config@ForwarderConfiguration{acceptorEndpoint} loQueue =
  tryJust excludeAsyncExceptions (connectToAcceptor iomgr config loQueue) >>= \case
    Left e -> do
      logTrace $ "trace-forward, connection with "
                 <> show acceptorEndpoint <> " failed: " <> show e
      sleep 1.0
      runTraceForwarder iomgr config loQueue
    Right _ -> return ()
 where
  excludeAsyncExceptions e =
    case fromException e of
      Just SomeAsyncException {} -> Nothing
      _ -> Just e
  logTrace = traceWith $ showTracing stdoutTracer
