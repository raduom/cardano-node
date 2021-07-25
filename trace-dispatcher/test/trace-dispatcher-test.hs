import           Test.Tasty

import           Cardano.Logging ()
import qualified Cardano.Logging.Test.Filtering as Filtering


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "trace-dispatcher"
    [ Filtering.tests
--    , Test.Rotator.tests
    ]
