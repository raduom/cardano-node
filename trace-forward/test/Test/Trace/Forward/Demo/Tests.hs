{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Trace.Forward.Demo.Tests
  ( tests
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue)
import           Control.Monad (void)
import           Data.Functor ((<&>))
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           System.Directory (getTemporaryDirectory)
#if defined(mingw32_HOST_OS)
import           System.FilePath ((</>), dropDrive)
import qualified Data.Text as T
#else
import           System.FilePath ((</>))
#endif
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.IOManager (withIOManager)

import           Trace.Forward.Acceptor
import           Trace.Forward.Configuration
import           Trace.Forward.Forwarder
import           Trace.Forward.Protocol.Type

import           Test.Trace.Forward.Demo.Configs
import           Test.Trace.Forward.Protocol.Codec ()
import           Test.Trace.Forward.Protocol.TraceItem

data Endpoint = Remote | Local

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Trace.Forward.Demo"
  [ testProperty "RemoteSocket" $ prop_RemoteSocket 100 Remote
  , testProperty "LocalPipe"    $ prop_RemoteSocket 100 Local
  ]

prop_RemoteSocket :: Int -> Endpoint -> Property
prop_RemoteSocket n ep' = ioProperty . withIOManager $ \iomgr -> do
  ep <- case ep' of
          Remote -> return $ RemoteSocket "127.0.0.1" 3030
          Local  -> LocalPipe <$> mkLocalPipePath

  forwarderQueue <- newTBQueueIO $ fromIntegral n
  acceptedItems :: IORef [TraceItem] <- newIORef []
  weAreDone <- newIORef False
  -- Run the acceptor. It will ask 'TraceItem's from the forwarder
  -- and store them in 'acceptorQueue'.
  void . forkIO $
    runTraceAcceptor
      iomgr
      (mkAcceptorConfig ep weAreDone)
      (traceItemsHandler acceptedItems)
      nodeInfoHandler
  threadDelay 1000000
  -- Run the forwarder. It will take 'TraceItem's from 'forwarderQueue'.
  void . forkIO $
    runTraceForwarder
      iomgr
      (mkForwarderConfig ep (return nodeInfo))
      forwarderQueue
  -- Generate 'n' arbitrary 'TraceItem's and write them in 'forwarderQueue'.
  itemsToForward <- generateNTraceItems n
  atomically $ mapM_ (writeTBQueue forwarderQueue) itemsToForward
  -- Just wait till the acceptor will ask and receive all 'TraceItem's from the forwarder.
  waitForFinish acceptedItems n weAreDone
  acceptedItems' <- readIORef acceptedItems
  -- Compare results.
  return $ itemsToForward === acceptedItems'

traceItemsHandler :: IORef [TraceItem] -> [TraceItem] -> IO ()
traceItemsHandler acceptedItems' items = do
  atomicModifyIORef' acceptedItems' $ \storedItems -> (storedItems ++ items, ())

nodeInfoHandler :: NodeInfo -> IO ()
nodeInfoHandler _ni = return ()

generateNTraceItems :: Int -> IO [TraceItem]
generateNTraceItems n = generate (infiniteListOf arbitrary) <&> take n

waitForFinish :: IORef [TraceItem] -> Int -> IORef Bool -> IO ()
waitForFinish acceptedItems' n weAreDone' = do
  items' <- readIORef acceptedItems'
  if length items' < n
    then threadDelay 1000 >> waitForFinish acceptedItems' n weAreDone'
    else atomicModifyIORef' weAreDone' $ const (True, ())

nodeInfo :: NodeInfo
nodeInfo = NodeInfo
  { niName            = "core-3"
  , niProtocol        = "Shelley"
  , niVersion         = "1.28.0"
  , niCommit          = "cffa06c"
  , niStartTime       = UTCTime (fromGregorian 2021 7 24) ((22 * 3600) + (15 * 60) +  1)
  , niSystemStartTime = UTCTime (fromGregorian 2017 9 24) (( 1 * 3600) + (44 * 60) + 51)
  }

mkLocalPipePath :: IO FilePath
mkLocalPipePath = do
  tmpDir <- getTemporaryDirectory
#if defined(mingw32_HOST_OS)
  return $ "\\\\.\\pipe\\" <> (T.unpack . T.replace "\\" "-" . T.pack) (dropDrive tmpDir)
                           <> "_" </> "trace-forward-test"
#else
  return $ tmpDir </> "trace-forward-test.sock"
#endif
