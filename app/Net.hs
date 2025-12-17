{-# LANGUAGE ScopedTypeVariables #-}

module Net
  ( netMain
  , ServerResources(..)
  , shutdownNet
  , sendMessage
  ) where

import Message ( Message(..), decodeMsg, encodeMsg )
import Options ( Options(..), IPVersion(..), ipVersionOpt )
import TUI ( ServerEvent(..), updateTUI )

import qualified Data.Set as S
import Data.Foldable ( traverse_ )

import Control.Concurrent ( forkIO, MVar, newMVar, modifyMVar_, readMVar )
import Control.Concurrent.STM
    ( atomically, newTVarIO, readTVar, readTVarIO, writeTVar, TVar, TChan )
import Control.Exception ( catch, IOException, throwIO, finally )
import Control.Monad ( forever, when )
import Network.Socket
    ( defaultHints, getAddrInfo, withSocketsDo, bind, socket, close,
      AddrInfo(addrFamily, addrSocketType, addrProtocol, addrAddress, addrFlags),
      AddrInfoFlag(AI_NUMERICSERV, AI_PASSIVE),
      Family(AF_INET, AF_INET6),
      PortNumber,
      SockAddr,
      Socket,
      SocketType(Datagram) )
import Network.Socket.ByteString ( recvFrom, sendTo )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B
import System.IO ( hFlush, stdout )
import Text.Printf ( printf )

data ServerResources = ServerResources
  { sockets :: MVar [Socket]
  , peersSeen :: TVar (S.Set SockAddr)
  , eventChan :: TChan ServerEvent
  }

-- | The main network entry point
netMain :: Options -> TChan ServerEvent -> TVar (S.Set SockAddr) -> IO ServerResources
netMain userOptions eventChan peersSeen =
  withSocketsDo $ do
    serverAddrs <- getExternalPortAddress (fromIntegral $ portNumOpt userOptions) (ipVersionOpt userOptions)
    socketsVar <- newMVar []

    let resources = ServerResources
          { sockets = socketsVar
          , peersSeen = peersSeen
          , eventChan = eventChan
          }

    -- Notify TUI of server addresses
    mapM_ (\addr -> updateTUI eventChan $ ServerStarted (show $ addrAddress addr)) serverAddrs

    -- Start server on each address
    mapM_ (\addr -> do
      sock <- connectAndRun resources addr (msgSizeOpt userOptions)
      modifyMVar_ socketsVar (\socks -> return (sock : socks))
      ) serverAddrs

    return resources

-- | Bind to an address and serve incoming messages
connectAndRun :: ServerResources -> AddrInfo -> Int -> IO Socket
connectAndRun resources addrInfo receiveSize = do
  sock <- socket
    (addrFamily addrInfo)
    (addrSocketType addrInfo)
    (addrProtocol addrInfo)
  bind sock (addrAddress addrInfo)
    `catch` (\e -> do
      updateTUI (eventChan resources) $ ParseError ("Failed to bind: " ++ show (e :: IOException)) 0
      throwIO e)

  -- Start receiving loop
  forkIO $ forever $ do
    (msgData, senderAddr) <- recvFrom sock receiveSize
      `catch` (\(_ :: IOException) -> do
        -- Ignore errors during shutdown
        return (B.empty, addrAddress addrInfo))

    when (B.length msgData > 0) $ do
      -- Check if this is a new peer
      isNew <- atomically $ do
        seen <- readTVar (peersSeen resources)
        if S.member senderAddr seen then
          return False
        else do
          writeTVar (peersSeen resources) (S.insert senderAddr seen)
          return True

      when isNew $ updateTUI (eventChan resources) $ PeerConnected senderAddr

      updateTUI (eventChan resources) $ MessageReceived senderAddr (B.length msgData)

      seen <- readTVarIO (peersSeen resources)
      _ <- forkIO $ processIncoming resources sock (BL.fromStrict msgData) seen
      return ()

  return sock

-- | Construct a list of candidate address information structures
-- Defaults to IPv6, but can be configured via IPVersion option
getExternalPortAddress :: PortNumber -> IPVersion -> IO [AddrInfo]
getExternalPortAddress port ipVersion = do
  let hints4 = defaultHints
        { addrFlags = [AI_PASSIVE, AI_NUMERICSERV]
        , addrSocketType = Datagram
        , addrFamily = AF_INET
        }
  let hints6 = defaultHints
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
processIncoming :: ServerResources -> Socket -> BL.ByteString -> S.Set SockAddr -> IO ()
processIncoming resources sock msgData seen =
  case decodeMsg msgData of
    Just msg -> do
      updateTUI (eventChan resources) $ MessageBroadcast (S.size seen)
      broadcastMsg resources sock (BL.toStrict $ encodeMsg msg) seen
    Nothing -> do
      let received = BL.take 200 msgData
      let preview = if BL.length msgData > 200 then BL.append received (BL.pack "...") else received
      let errorMsg = "Failed to decode JSON: " ++ BL.unpack preview
      updateTUI (eventChan resources) $ ParseError errorMsg (fromIntegral $ BL.length msgData)

-- | Broadcast a message to all known peers
broadcastMsg :: ServerResources -> Socket -> B.ByteString -> S.Set SockAddr -> IO ()
broadcastMsg resources sock msgData seen =
  traverse_ (sendTo sock msgData) seen
    `catch` (\(_ :: IOException) -> return ())  -- Ignore send errors

-- | Send a message to all connected peers
-- Maximum message length is 256 characters
sendMessage :: ServerResources -> String -> IO ()
sendMessage resources msgText = do
  socks <- readMVar (sockets resources)
  peers <- readTVarIO (peersSeen resources)

  when (not (S.null peers) && not (null socks)) $ do
    -- Truncate message to 256 characters max
    let maxLen = 256
    let truncatedMsg = take maxLen msgText

    -- Create message with to=0 (broadcast to all)
    let msg = Message { to = 0, msg = truncatedMsg }
    let msgData = BL.toStrict $ encodeMsg msg

    -- Send on first socket (they're all bound to same port)
    traverse_ (\sock ->
      traverse_ (sendTo sock msgData) peers
        `catch` (\(_ :: IOException) -> return ())
      ) socks

-- | Clean shutdown - close all sockets
shutdownNet :: ServerResources -> IO ()
shutdownNet resources = do
  socks <- readMVar (sockets resources)
  mapM_ (\sock -> catch (close sock) (\(_ :: IOException) -> return ())) socks
  updateTUI (eventChan resources) ServerStopped
