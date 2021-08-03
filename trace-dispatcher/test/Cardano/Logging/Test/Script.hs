{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Test.Script
 (
    playScript
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import           Data.IORef (newIORef, readIORef)
import           Data.List (sort)

import           Cardano.Logging
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Tracer

import           Debug.Trace

-- | Adds a message id to every message.
-- MessageId gives the id to start with.
-- Returns a tuple with the messages with ids and
-- the successor of the last used messageId
withMessageIds :: MessageID -> [ScriptedMessage] -> ([ScriptedMessage], MessageID)
withMessageIds mid sMsgs = go mid sMsgs []
  where
    go mid' [] acc = (reverse acc, mid')
    go mid' (ScriptedMessage time msg : tl) acc =
      go (mid' + 1) tl (ScriptedMessage time (setMessageID msg mid') : acc)

-- | Duration of the test is always ten seconds
playScript :: TraceConfig -> Script -> IO ([FormattedMessage],[FormattedMessage],[FormattedMessage])
playScript config (Script msgs) = do
  stdoutTrRef     <- newIORef []
  stdoutTracer'   <- testTracer stdoutTrRef
  forwardTrRef    <- newIORef []
  forwardTracer'  <- testTracer forwardTrRef
  ekgTrRef        <- newIORef []
  ekgTracer'      <- testTracer ekgTrRef
  tr              <- mkCardanoTracer
                      "Cardano"
                      namesForMessage
                      severityForMessage
                      privacyForMessage
                      stdoutTracer'
                      forwardTracer'
                      (Just ekgTracer')
  let sortedMsgs = sort msgs
  let (msgsWithIds,_) = withMessageIds 0 sortedMsgs

  configureTracers config docMessage [tr]
  trace ("playScript " <> show msgsWithIds) $
    playIt (Script msgsWithIds) tr 0.0
  r1 <- readIORef stdoutTrRef
  r2 <- readIORef forwardTrRef
  r3 <- readIORef ekgTrRef
  pure (reverse r1, reverse r2, reverse r3)


-- | Play the current script in one thread
-- The time is in milliseconds
playIt :: Script -> Trace IO Message -> Double -> IO ()
playIt (Script []) _tr _d = pure ()
playIt (Script (ScriptedMessage d1 m1 : rest)) tr d = do
  when (d < d1) $
    threadDelay (round (d1 - d) * 1000)
  traceWith tr m1
  playIt (Script rest) tr d1
