{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Test.Filtering (
    propFiltering
  ) where

import           Test.Tasty.QuickCheck

import           Cardano.Logging
import           Cardano.Logging.Test.Messages
import           Cardano.Logging.Test.Script

import           Debug.Trace



propFiltering :: TraceConfig -> Property
propFiltering conf = do
  let scriptGen  :: Gen Script = arbitrary
  forAll scriptGen (\ script -> ioProperty $ do
    (stdoutTrRes, forwardTrRes, ekgTrRes) <- playScript conf script
    trace ("stdoutTrRes " <> show stdoutTrRes
            <> " forwardTrRes " <> show forwardTrRes
            <> " ekgTrRes " <> show ekgTrRes) $
      pure True)
