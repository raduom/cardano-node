module Cardano.Logging.Test.Config (
    standardConfig
  ) where

import Cardano.Logging
import Data.Map(fromList)

standardConfig :: TraceConfig
standardConfig = emptyTraceConfig {
  tcOptions = fromList
    [([] :: Namespace,
         [ CoSeverity DebugF
         , CoDetail DNormal
         , CoBackend [Stdout HumanFormatColoured, Forwarder, EKGBackend]
         ])
    ]
  }
