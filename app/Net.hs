{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Net (
  netMain,
  ServerResources (..),
  shutdownNet,
  shutdownNetIO,
  sendMessage,
  sendMessageIO,
  AppEnv (..),
  NetM (..),
  NetMProd,
  connectAndRun,
  runNetM,
) where

import Message (Message (..), BeaconMessage (..), decodeMsg, decodeBeacon, encodeMsg, encodeBeacon)
import MonadNet (MonadNet (..))
import Options (IPVersion (..), Options (..), ipVersionOpt)
import TUI (ServerEvent (..), updateTUI)

import Data.Foldable (traverse_)
import qualified Data.Set as S

import Control.Concurrent (MVar, ThreadId, forkIO, modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Concurrent.STM (
  TChan,
  TVar,
  atomically,
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTVar,
 )
import Control.Exception (IOException, catch, throwIO)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Logger (
  LoggingT,
  MonadLogger,
  defaultLogStr,
  fromLogStr,
  logDebugN,
  logErrorN,
  logInfoN,
  runLoggingT,
  toLogStr,
 )
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, liftIO, runReaderT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Network.Socket (
  AddrInfo (addrAddress, addrFamily, addrFlags, addrProtocol, addrSocketType),
  AddrInfoFlag (AI_NUMERICSERV, AI_PASSIVE),
  Family (AF_INET, AF_INET6),
  HostAddress,
  PortNumber,
  SockAddr (..),
  Socket,
  SocketOption (Broadcast),
  SocketType (Datagram),
  bind,
  close,
  defaultHints,
  getAddrInfo,
  setSocketOption,
  socket,
  tupleToHostAddress,
  withSocketsDo,
 )
import Network.Socket.ByteString (sendTo)
import qualified Network.Socket.ByteString as NSB
import System.IO (
  BufferMode (LineBuffering),
  Handle,
  IOMode (AppendMode),
  hClose,
  hFlush,
  hSetBuffering,
  openFile,
  withFile,
 )

{- | Resources needed for the server to operate
's' will be 'Socket' in production, and 'Int' (or other mock type) in testing
-}
data ServerResources s = ServerResources
  { sockets :: MVar [s]
  , peersSeen :: TVar (S.Set SockAddr)
  , eventChan :: TChan ServerEvent
  , logHandle :: Handle
  -- ^ Log file handle, kept open for the lifetime of the network subsystem
  }

{- | The Read-Only Environment
Parameterized by socket type for full polymorphism
-}
data AppEnv s = AppEnv
  { envResources :: ServerResources s
  , envOptions :: Options
  }

{- | Custom Application Monad
New Stack: Reader -> Logger -> IO
This wraps IO and gives us access to AppEnv everywhere, plus structured logging
Parameterized by socket type for polymorphism
-}
newtype NetM s a = NetM {runNetM' :: ReaderT (AppEnv s) (LoggingT IO) a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader (AppEnv s)
    , MonadLogger
    , MonadIO
    , MonadUnliftIO
    )

{- | Production instance: Real sockets over IO
Type alias for production use
-}
type NetMProd = NetM Socket

instance MonadNet NetMProd where
  type NetSocket NetMProd = Socket

  createSocket addr =
    NetM . liftIO $
      socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

  bindSocket sock addr =
    NetM . liftIO $
      bind sock addr

  closeSocket sock =
    NetM . liftIO $
      close sock

  receiveFrom sock size =
    NetM . liftIO $
      NSB.recvFrom sock size
        `catch` ( \(_ :: IOException) -> do
                    -- Return a safe dummy address instead of undefined
                    -- Use SockAddrInet constructor to create a valid but unused address (0.0.0.0:0)
                    -- This won't crash if printed or used in comparisons
                    -- Note: We check for empty messages before using the address, so this is safe
                    let dummyAddr = SockAddrInet 0 (tupleToHostAddress (0, 0, 0, 0))
                    return (B.empty, dummyAddr)
                )

  sendToPeer sock msg addr =
    NetM . liftIO $
      void $
        NSB.sendTo sock msg addr

{- | Helper to run our Monad
We write logs to a file to avoid breaking the TUI
The handle is kept open in ServerResources for the lifetime of the network subsystem
-}
runNetM :: AppEnv s -> NetM s a -> IO a
runNetM env (NetM action) = do
  let h = logHandle (envResources env)
  -- We define a custom runner that pushes logs to the file handle
  let logRunner loc source level str = do
        let logMsg = defaultLogStr loc source level str
        B.hPutStr h (fromLogStr logMsg)
        B.hPutStr h "\n"
        hFlush h
  runLoggingT (runReaderT action env) logRunner

-- | Helper to update TUI (User facing events)
emitEvent :: (MonadReader (AppEnv s) m, MonadIO m) => ServerEvent -> m ()
emitEvent evt = do
  chan <- asks (eventChan . envResources)
  liftIO $ updateTUI chan evt

{- | The main network entry point
Returns ServerResources parameterized by Socket for production use
-}
netMain :: Options -> TChan ServerEvent -> TVar (S.Set SockAddr) -> IO (ServerResources Socket)
netMain userOptions eventChan peersSeen = withSocketsDo $ do
  serverAddrs <-
    getExternalPortAddress (fromIntegral $ portNumOpt userOptions) (ipVersionOpt userOptions)
  socketsVar <- newMVar []

  -- Open the log file handle and keep it open for the lifetime of the network subsystem
  logH <- openFile (logFileOpt userOptions) AppendMode
  hSetBuffering logH LineBuffering

  let resources =
        ServerResources
          { sockets = socketsVar
          , peersSeen = peersSeen
          , eventChan = eventChan
          , logHandle = logH
          }

  let env = AppEnv resources userOptions

  -- Run the setup logic inside our ReaderT monad
  runNetM env $ do
    logInfoN "Starting Network Subsystem..."

    traverse_
      ( \addr -> do
          logInfoN $ T.pack $ "Binding to " ++ show (addrAddress addr)
          emitEvent $ ServerStarted (show $ addrAddress addr)
      )
      serverAddrs

    traverse_
      ( \addr -> do
          sock <- connectAndRun addr
          -- Set socket options for beaconing (production Socket only)
          -- IPv4: Enable broadcast for sending to 255.255.255.255
          -- IPv6: No special options needed for sending to multicast addresses
          liftIO $ case addrFamily addr of
            AF_INET -> setSocketOption (sock :: Socket) Broadcast 1
            _ -> return ()
          liftIO $ modifyMVar_ socketsVar (\socks -> return (sock : socks))
      )
      serverAddrs

    -- Start beacon sender if enabled
    -- Note: beaconEnabledOpt is inverted - True means disable-beacon was set
    -- So we check 'not' to enable beaconing when the flag is NOT set
    when (not $ beaconEnabledOpt userOptions) $ do
      void $ liftIO $ startBeaconing resources userOptions serverAddrs

  return resources

{- | Bind to an address and serve incoming messages
Note: Now polymorphic over any MonadNet, MonadReader, MonadLogger, MonadUnliftIO
-}
connectAndRun ::
  (MonadNet m, MonadReader (AppEnv (NetSocket m)) m, MonadLogger m, MonadUnliftIO m) =>
  AddrInfo ->
  m (NetSocket m)
connectAndRun addrInfo = do
  env <- ask
  let receiveSize = msgSizeOpt (envOptions env)

  -- 1. Abstract Creation
  sock <- createSocket addrInfo

  -- 2. Abstract Binding
  -- Note: Exception handling would require MonadCatch from exceptions package
  -- For now, let bind failures propagate (they'll be caught at a higher level)
  bindSocket sock (addrAddress addrInfo)

  -- 3. Abstract Looping
  -- We use 'withRunInIO' from MonadUnliftIO to fork
  withRunInIO $ \run -> do
    forkIO $ run $ forever $ do
      -- Exception handling is done in the receiveFrom implementation
      (msgData, senderAddr) <- receiveFrom sock receiveSize

      when (B.length msgData > 0) $ do
        -- Debug log for every packet (great for dev, invisible to user)
        logDebugN $ T.pack $ "Received " ++ show (B.length msgData) ++ " bytes from " ++ show senderAddr
        handleIncomingMessage sock msgData senderAddr

  return sock

handleIncomingMessage ::
  (MonadNet m, MonadReader (AppEnv (NetSocket m)) m, MonadLogger m, MonadUnliftIO m) =>
  NetSocket m ->
  B.ByteString ->
  SockAddr ->
  m ()
handleIncomingMessage sock msgData senderAddr = do
  resources <- asks envResources

  -- Check if this is a beacon message
  case decodeBeacon (BL.fromStrict msgData) of
    Just beacon -> do
      -- This is a beacon, add sender to peers if new
      isNew <- liftIO $ atomically $ do
        seen <- readTVar (peersSeen resources)
        if S.member senderAddr seen
          then return False
          else do
            writeTVar (peersSeen resources) (S.insert senderAddr seen)
            return True

      when isNew $ do
        logInfoN $ T.pack $ "Peer discovered via beacon: " ++ show senderAddr
        emitEvent $ PeerDiscovered senderAddr
      return () -- Don't process beacon as regular message

    Nothing -> do
      -- Regular message processing
      -- Check if this is a new peer
      isNew <- liftIO $ atomically $ do
        seen <- readTVar (peersSeen resources)
        if S.member senderAddr seen
          then return False
          else do
            writeTVar (peersSeen resources) (S.insert senderAddr seen)
            return True

      when isNew $ do
        logInfoN $ T.pack $ "New Peer Discovered: " ++ show senderAddr
        emitEvent $ PeerConnected senderAddr

      let msgRecipientId = maybe 0 to (decodeMsg (BL.fromStrict msgData))
      emitEvent $ MessageReceived senderAddr msgRecipientId (B.length msgData)

      -- Process logic for regular messages
      -- We snapshot the peers here to avoid holding the lock during processing
      peersSnapshot <- liftIO $ readTVarIO (peersSeen resources)

      -- We fork the processing to keep the listener loop tight
      withRunInIO $ \run ->
        void $
          forkIO $
            run $
              processIncoming sock (BL.fromStrict msgData) senderAddr peersSnapshot

{- | Construct a list of candidate address information structures
Defaults to IPv6, but can be configured via IPVersion option
-}
getExternalPortAddress :: PortNumber -> IPVersion -> IO [AddrInfo]
getExternalPortAddress port ipVersion = do
  let hints4 =
        defaultHints
          { addrFlags = [AI_PASSIVE, AI_NUMERICSERV]
          , addrSocketType = Datagram
          , addrFamily = AF_INET
          }
  let hints6 =
        defaultHints
          { addrFlags = [AI_PASSIVE, AI_NUMERICSERV]
          , addrSocketType = Datagram
          , addrFamily = AF_INET6
          }

  case ipVersion of
    IPv4 -> getAddrInfo (Just hints4) Nothing (Just $ show port)
    IPv6 -> getAddrInfo (Just hints6) Nothing (Just $ show port)
    Both -> do
      addrs4 <- getAddrInfo (Just hints4) Nothing (Just $ show port)
      addrs6 <- getAddrInfo (Just hints6) Nothing (Just $ show port)
      -- Return IPv6 first (default), then IPv4
      return $ addrs6 ++ addrs4

-- | Parse incoming message and conditionally broadcast
processIncoming ::
  (MonadNet m, MonadReader (AppEnv (NetSocket m)) m, MonadLogger m, MonadIO m) =>
  NetSocket m ->
  BL.ByteString ->
  SockAddr ->
  S.Set SockAddr ->
  m ()
processIncoming sock msgData senderAddr seen =
  case decodeMsg msgData of
    Just decodedMsg -> do
      logInfoN $ "Broadcasting valid message to " <> T.pack (show (S.size seen)) <> " peers"
      emitEvent $ MessageBroadcast senderAddr (to decodedMsg) (msg decodedMsg) (S.size seen)
      broadcastMsg sock (BL.toStrict $ encodeMsg decodedMsg) seen
    Nothing -> do
      let received = BL.take 200 msgData
      let errorMsg = "Failed to decode: " ++ BL.unpack received

      -- We log the error to the file for the dev
      logErrorN $ T.pack errorMsg
      -- And we show the error in the TUI for the user
      emitEvent $ ParseError errorMsg (fromIntegral $ BL.length msgData)

-- | Broadcast a message to all known peers
broadcastMsg :: (MonadNet m, MonadIO m) => NetSocket m -> B.ByteString -> S.Set SockAddr -> m ()
broadcastMsg sock msgData =
  mapM_ (sendToPeer sock msgData)

{- | Start the beacon sender thread
Periodically sends beacon messages to discover peers on the LAN
-}
startBeaconing :: ServerResources Socket -> Options -> [AddrInfo] -> IO ThreadId
startBeaconing resources opts addrs = do
  let interval = beaconIntervalOpt opts * 1000000 -- Convert to microseconds
  let port = fromIntegral $ portNumOpt opts

  -- Create beacon message
  let beacon = BeaconMessage
        { beaconType = "beacon"
        , beaconVersion = "1.0"
        , beaconPort = fromIntegral port
        , beaconTimestamp = 0 -- Placeholder, can be enhanced later
        }
  let beaconData = BL.toStrict $ encodeBeacon beacon

  -- Create target addresses for each socket
  targetAddrs <- mapM (createBeaconTarget port) addrs

  forkIO $ forever $ do
    socks <- readMVar (sockets resources)
    -- Send beacon on each socket to its corresponding target address
    mapM_ (\(sock, targetAddr) ->
      catch (void $ sendTo sock beaconData targetAddr)
            (\(_ :: IOException) -> return ())
      ) (zip socks targetAddrs)
    threadDelay interval

{- | Create the target address for beaconing based on address family
IPv4: broadcast address (255.255.255.255)
IPv6: all-nodes multicast (ff02::1)
-}
createBeaconTarget :: PortNumber -> AddrInfo -> IO SockAddr
createBeaconTarget port addr = case addrFamily addr of
  AF_INET -> return $ SockAddrInet port (tupleToHostAddress (255, 255, 255, 255))
  AF_INET6 -> do
    -- Resolve IPv6 multicast address ff02::1
    addrs <- getAddrInfo (Just defaultHints { addrFamily = AF_INET6 }) (Just "ff02::1") (Just $ show port)
    case addrs of
      (a : _) -> return $ addrAddress a
      [] -> return $ addrAddress addr -- Fallback
  _ -> return $ addrAddress addr -- Fallback to original address

{- | Send a message to all connected peers
Maximum message length is 256 characters
-}
sendMessage :: (MonadNet m, MonadIO m) => ServerResources (NetSocket m) -> String -> m ()
sendMessage resources msgText = do
  socks <- liftIO $ readMVar (sockets resources)
  peers <- liftIO $ readTVarIO (peersSeen resources)

  when (not (S.null peers) && not (null socks)) $ do
    -- Truncate message to 256 characters max
    let maxLen = 256
    let truncatedMsg = take maxLen msgText

    -- Create message with to=0 (broadcast to all)
    let msg = Message{to = 0, msg = truncatedMsg}
    let msgData = BL.toStrict $ encodeMsg msg

    -- Use the abstract 'sendToPeer' from MonadNet
    traverse_ (\sock -> traverse_ (sendToPeer sock msgData) peers) socks

{- | Production wrapper: runs sendMessage in IO context
This is needed for compatibility with TUI which expects (String -> IO ())
-}
sendMessageIO :: ServerResources Socket -> String -> IO ()
sendMessageIO resources msgText = do
  -- Create a dummy AppEnv for running NetMProd
  -- SAFE: We don't actually use Options in sendMessage, so undefined is safe here
  -- The Options constructor is not exported, so we use undefined as a workaround
  let dummyEnv = AppEnv resources undefined
  runNetM dummyEnv $ sendMessage resources msgText

{- | Clean shutdown - close all sockets
Now polymorphic: works with both production and test environments
-}
shutdownNet :: (MonadNet m, MonadIO m) => ServerResources (NetSocket m) -> m ()
shutdownNet resources = do
  socks <- liftIO $ readMVar (sockets resources)
  traverse_ closeSocket socks
  liftIO $ updateTUI (eventChan resources) ServerStopped

-- | Production wrapper: runs shutdownNet in IO context
shutdownNetIO :: ServerResources Socket -> IO ()
shutdownNetIO resources = do
  -- SAFE: We don't actually use Options in shutdownNet, so undefined is safe here
  let dummyEnv = AppEnv resources undefined
  runNetM dummyEnv $ shutdownNet resources
  -- Close the log file handle
  hClose (logHandle resources)
