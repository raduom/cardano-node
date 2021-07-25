{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Test.Filtering (
    tests
  ) where

import           Control.Monad (when)
import           Control.Concurrent (threadDelay)
import           Data.IORef (newIORef, readIORef)
import           Data.List (sort)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Config
import           Cardano.Logging.Test.Tracer

import           Debug.Trace

tests :: TestTree
tests = localOption (QuickCheckTests 3) $ testGroup "Test.Filtering"
  [ testProperty "not-filtered" $ propFiltering standardConfig
  ]

propFiltering :: TraceConfig -> Property
propFiltering conf = do
  let scriptGen  :: Gen Script = arbitrary
  forAll scriptGen (\ script -> ioProperty $ do
    (stdoutTrRes, forwardTrRes, ekgTrRes) <- playScript conf script
    trace ("stdoutTrRes " <> show stdoutTrRes
            <> " forwardTrRes " <> show forwardTrRes
            <> " ekgTrRes " <> show ekgTrRes) $
      pure True)


-- | Duration of the test is always ten seconds
playScript :: TraceConfig -> Script -> IO ([FormattedMessage],[FormattedMessage],[FormattedMessage])
playScript config (Script l) = trace ("playScript " <> show (sort l)) $ do
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
  let sortedScript = Script (sort l)
  configureTracers config docMessage [tr]
  playIt sortedScript tr 0.0
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
