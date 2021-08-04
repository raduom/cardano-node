{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Test.Script
  (
    runScriptSimple
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import           Data.IORef (newIORef, readIORef)
import           Data.List (sort)
import           Test.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Tracer
import           Cardano.Logging.Test.Types

import           Debug.Trace

runScriptSimple ::
     TraceConfig
  -> (TraceConfig -> ScriptRes -> Property)
  -> Double
  -> Property
runScriptSimple conf oracle time = do
  let scriptGen  :: Gen Script = arbitrary
  forAll scriptGen (\ script -> ioProperty $ do
    scriptResult <- playScript conf script time
    trace ("stdoutTrRes " <> show (srStdoutRes scriptResult)
            <> " forwardTrRes " <> show (srForwardRes scriptResult)
            <> " ekgTrRes " <> show (srEkgRes scriptResult)) $
      pure $ oracle conf scriptResult)

-- | Duration of the test is given by time in seconds
playScript :: TraceConfig -> Script -> Double -> IO ScriptRes
playScript config (Script msgs) time = do
  stdoutTrRef     <- newIORef []
  stdoutTracer'   <- testTracer stdoutTrRef
  forwardTrRef    <- newIORef []
  forwardTracer'  <- testTracer forwardTrRef
  ekgTrRef        <- newIORef []
  ekgTracer'      <- testTracer ekgTrRef
  tr              <- mkCardanoTracer
                      "Test"
                      namesForMessage
                      severityForMessage
                      privacyForMessage
                      stdoutTracer'
                      forwardTracer'
                      (Just ekgTracer')
  let sortedMsgs = sort msgs
  let (msgsWithIds,_) = withMessageIds 0 sortedMsgs
  let timedMessages = map (withTimeFactor time) msgsWithIds

  configureTracers config docMessage [tr]
  trace ("playScript " <> show timedMessages) $
    playIt (Script timedMessages) tr 0.0
  r1 <- readIORef stdoutTrRef
  r2 <- readIORef forwardTrRef
  r3 <- readIORef ekgTrRef
  pure (ScriptRes
          (Script timedMessages)
          (reverse r1)
          (reverse r2)
          (reverse r3))

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

withTimeFactor :: Double -> ScriptedMessage -> ScriptedMessage
withTimeFactor factor (ScriptedMessage time msg) =
    ScriptedMessage (time * factor) msg

-- | Play the current script in one thread
-- The time is in milliseconds
playIt :: Script -> Trace IO Message -> Double -> IO ()
playIt (Script []) _tr _d = pure ()
playIt (Script (ScriptedMessage d1 m1 : rest)) tr d = do
  when (d < d1) $
    threadDelay (round (d1 - d) * 1000)
  traceWith tr m1
  playIt (Script rest) tr d1
