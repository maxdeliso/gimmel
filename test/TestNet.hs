{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Message (Message (..), encodeMsg)
import MonadNet (MonadNet (..))
import Net (AppEnv (..), ServerResources (..), sendMessage)
import Options (IPVersion (IPv4), Options (..))
import TUI (ServerEvent (..))

import Control.Concurrent (MVar, forkIO, killThread, modifyMVar_, newMVar)
import Control.Concurrent.STM (
  TChan,
  TMVar,
  TQueue,
  TVar,
  atomically,
  modifyTVar',
  newEmptyTMVarIO,
  newTChanIO,
  newTQueue,
  newTVarIO,
  putTMVar,
  readTChan,
  readTQueue,
  readTVar,
  retry,
  takeTMVar,
  writeTChan,
  writeTQueue,
  writeTVar,
 )
import Control.Monad (forever, when)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Logger (LoggingT, MonadLogger, runLoggingT, runNoLoggingT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, liftIO, local, runReaderT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Map as M
import qualified Data.Set as S
import Network.Socket (
  AddrInfo (..),
  Family (AF_INET),
  PortNumber,
  SockAddr (..),
  SocketType (Datagram),
  tupleToHostAddress,
 )
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Timeout (timeout)

-- | The "Internet" is just a map of Addresses to Message Queues
type VirtualInternet = TVar (M.Map SockAddr (TQueue (B.ByteString, SockAddr)))

{- | The Test Monad - uses AppEnv directly so connectAndRun works
We store virtual internet and address in IORefs
-}
data TestEnv = TestEnv
  { testAppEnv :: AppEnv SockAddr
  , testInternet :: IORef VirtualInternet
  , testMyAddr :: SockAddr
  }

newtype TestM a = TestM {unTestM :: ReaderT TestEnv (LoggingT IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadUnliftIO)

-- Note: We can't make TestM an instance of MonadReader (AppEnv SockAddr) due to
-- functional dependency conflicts. Instead, we'll create a test-specific connectAndRun
-- that extracts AppEnv and runs connectAndRun in the proper context.

{- | Test-specific version of connectAndRun that works with TestM
We reimplement the logic here since we can't use the polymorphic connectAndRun
-}
testConnectAndRun :: AddrInfo -> TestM SockAddr
testConnectAndRun addrInfo = do
  receiveSize <- TestM $ asks (msgSizeOpt . envOptions . testAppEnv)

  -- 1. Create socket (just return the address)
  sock <- createSocket addrInfo

  -- 2. Bind socket
  bindSocket sock (addrAddress addrInfo)

  -- 3. Start listener loop in background
  withRunInIO $ \run -> do
    forkIO $ run $ forever $ do
      (msgData, senderAddr) <- receiveFrom sock receiveSize

      when (B.length msgData > 0) $ do
        -- Handle incoming message (simplified - just emit event)
        TestM $ do
          env' <- ask
          let res = envResources (testAppEnv env')
          liftIO $ atomically $ writeTChan (eventChan res) (MessageReceived senderAddr 0 (B.length msgData))

  return sock

-- | Helper to get virtual internet
getInternet :: TestM VirtualInternet
getInternet = TestM $ do
  ref <- asks testInternet
  liftIO $ readIORef ref

-- | Helper to get my address
getMyAddr :: TestM SockAddr
getMyAddr = TestM $ asks testMyAddr

instance MonadNet TestM where
  type NetSocket TestM = SockAddr -- In tests, a socket is identified just by its address

  createSocket _ = do
    -- Return the address bound to this environment
    getMyAddr

  bindSocket addr _ = do
    -- Create an inbox for this address in the Virtual Internet
    net <- getInternet
    liftIO $ atomically $ do
      m <- readTVar net
      q <- newTQueue
      writeTVar net (M.insert addr q m)

  closeSocket addr = do
    -- Remove inbox from Internet
    net <- getInternet
    liftIO $ atomically $ modifyTVar' net (M.delete addr)

  sendToPeer _ msg targetAddr = do
    net <- getInternet
    me <- getMyAddr
    liftIO $ atomically $ do
      m <- readTVar net
      case M.lookup targetAddr m of
        Nothing -> return () -- Simulate packet loss / unreachable host
        Just q -> writeTQueue q (msg, me)

  receiveFrom _ _ = do
    net <- getInternet
    me <- getMyAddr
    liftIO $ atomically $ do
      m <- readTVar net
      case M.lookup me m of
        Nothing -> retry -- Socket not bound yet
        Just q -> do
          -- Block until a message arrives (simulating socket blocking)
          (msg, sender) <- readTQueue q
          return (msg, sender)

-- | Helper to run TestM
runTestM :: TestEnv -> TestM a -> IO a
runTestM env (TestM action) = do
  -- Use stdout for logging in tests (or we could use runNoLoggingT)
  hSetBuffering stdout LineBuffering
  let logRunner _loc _source _level _str = return () -- Suppress logs
  runLoggingT (runReaderT action env) (\_loc _source _level _str -> return ())

-- | Bootstraps a virtual node and runs a setup action
withVirtualNode :: VirtualInternet -> PortNumber -> (ServerResources SockAddr -> TestM a) -> IO a
withVirtualNode net port action = do
  let addr = SockAddrInet port (tupleToHostAddress (127, 0, 0, 1))

  -- Setup standard resources
  chan <- newTChanIO
  peersVar <- newTVarIO S.empty
  socketVar <- newMVar [] -- In tests, we don't really use this list of sockets much

  -- Dummy options
  let opts =
        MkOptions
          { portNumOpt = fromIntegral port
          , ipVersionOpt = IPv4
          , msgSizeOpt = 1024
          , logFileOpt = "test.log"
          }

  -- Use stdout for the log file handle in tests
  let resources = ServerResources socketVar peersVar chan stdout
  let appEnv = AppEnv resources opts

  netRef <- newIORef net
  let testEnv = TestEnv appEnv netRef addr

  runTestM testEnv $ action resources

-- | Create a dummy AddrInfo for testing
dummyAddrInfo :: SockAddr -> AddrInfo
dummyAddrInfo addr =
  AddrInfo
    { addrFlags = []
    , addrFamily = AF_INET
    , addrSocketType = Datagram
    , addrProtocol = 0
    , addrAddress = addr
    , addrCanonName = Nothing
    }

main :: IO ()
main = do
  putStrLn "Running Virtual Network Test..."

  -- 1. Initialize the empty "Internet"
  internet <- newTVarIO M.empty

  -- 2. Define Addresses
  let portA = 3000
  let portB = 3001
  let addrA = SockAddrInet portA (tupleToHostAddress (127, 0, 0, 1))
  let addrB = SockAddrInet portB (tupleToHostAddress (127, 0, 0, 1))

  -- Use a shared TChan for assertions
  resultChan <- newTChanIO

  -- COORDINATION: A flag to signal when Node B is actually bound and listening
  nodeBReady <- newEmptyTMVarIO

  -- START NODE B (The Listener)
  threadB <- forkIO $ withVirtualNode internet portB $ \resB -> do
    -- B: Bind to port and start listening
    let addrInfoB = dummyAddrInfo addrB
    _ <- testConnectAndRun addrInfoB

    -- SIGNAL READY: B has bound its socket. It is now safe for A to send.
    liftIO $ atomically $ putTMVar nodeBReady ()

    -- Wait for the event
    liftIO $ forever $ do
      evt <- atomically $ readTChan (eventChan resB)
      case evt of
        MessageReceived sender msgId len ->
          atomically $ writeTChan resultChan (sender, len)
        _ -> return ()

  -- SYNCHRONIZATION: Block main thread until Node B signals it is ready.
  -- This replaces 'threadDelay 100000' with a precise wait.
  atomically $ takeTMVar nodeBReady

  -- START NODE A (The Sender)
  withVirtualNode internet portA $ \resA -> do
    -- A: Bind to port and start listening
    let addrInfoA = dummyAddrInfo addrA
    sockA <- testConnectAndRun addrInfoA

    -- A: Add the socket to the sockets list (required for sendMessage)
    liftIO $ modifyMVar_ (sockets resA) (\socks -> return (sockA : socks))

    -- A: Manually add B as a known peer (simulating discovery)
    liftIO $ atomically $ writeTVar (peersSeen resA) (S.fromList [addrB])

    -- No wait needed here; sendMessage writes atomically to the queue
    let payload = "Ping"
    sendMessage resA payload
    liftIO $ putStrLn "Node A sent message."

  -- VERIFICATION
  -- We use 'timeout' as a safety net. If logic breaks, we fail after 1s.
  -- Otherwise, this returns instantly upon message receipt.
  result <- timeout 1000000 $ atomically $ readTChan resultChan

  killThread threadB

  case result of
    Just (sender, len) -> do
      if sender == addrA && len > 0
        then do
          putStrLn $ "SUCCESS: Node B received message from Node A! (length: " ++ show len ++ ")"
          exitSuccess
        else do
          putStrLn $ "FAILURE: Unexpected message: sender=" ++ show sender
          exitFailure
    Nothing -> do
      putStrLn "FAILURE: Timed out waiting for message (Deadlock or Packet Loss)"
      exitFailure
