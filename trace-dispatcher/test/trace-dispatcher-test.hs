import           Test.Tasty

import           Cardano.Logging ()

import           Test.Tasty.QuickCheck

import           Cardano.Logging.Test.Config
import           Cardano.Logging.Test.Filtering


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = localOption (QuickCheckTests 3) $ testGroup "trace-dispatcher"
    [ testProperty "not-filtered" $ propFiltering standardConfig
    ]
