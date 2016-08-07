module Net
  ( netMain
  ) where

import Message ( Message, decodeMsg, encodeMsg )
import Options ( Options(..) )

import qualified Data.Set as S

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
    ( atomically, newTVarIO, readTVar, readTVarIO, writeTVar, TVar )
import Control.Monad ( unless, forever )
import Network.Socket
    ( defaultHints, getAddrInfo, withSocketsDo, bind, socket,
      AddrInfo(addrFlags, addrFamily, addrSocketType, addrProtocol, addrAddress),
      AddrInfoFlag(AI_NUMERICSERV, AI_PASSIVE),
      SockAddr,
      Socket,
      SocketType(Datagram) )
import Network.Socket.ByteString ( recvFrom, sendTo )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B
import System.IO.Unsafe ( unsafePerformIO )

-- the main network monad
netMain :: Options -> IO ()
netMain userOptions =
  withSocketsDo $ do
    serverAddrs <- getCandidateAddresses $ show $ portNumOpt userOptions
    mapM_ connectAndRun $ zip serverAddrs $ replicate (length serverAddrs) (msgSizeOpt userOptions)

-- bind and serve
connectAndRun :: (AddrInfo, Int) -> IO ()
connectAndRun (addrInfo, receiveSize) =
  do
    sock <- socket
      (addrFamily addrInfo)
      (addrSocketType addrInfo)
      (addrProtocol addrInfo)
    bind sock (addrAddress addrInfo)
    forever $ do
      (msgData, senderAddr) <- recvFrom sock receiveSize
      atomically $ do
        seen <- readTVar peersSeen
        writeTVar peersSeen (S.insert senderAddr seen)
      seen <- readTVarIO peersSeen
      forkIO $ processIncoming sock (BL.fromStrict msgData) seen

-- a transactional variable holding a set of peer socket addresses
{-# NOINLINE peersSeen #-}
peersSeen :: TVar (S.Set SockAddr)
peersSeen = unsafePerformIO $ newTVarIO S.empty

-- construct a list of candidate address information structures
getCandidateAddresses :: String -> IO [AddrInfo]
getCandidateAddresses portString =
  getAddrInfo
    (Just
       defaultHints
       {addrFlags = [AI_PASSIVE, AI_NUMERICSERV], addrSocketType = Datagram})
    Nothing
    (Just $ portString)

-- given an incoming lazy bytestring, parse it and conditionally broadcast
processIncoming :: Socket -> BL.ByteString -> S.Set SockAddr -> IO ()
processIncoming sock msgData seen =
  case decodeMsg msgData :: Maybe Message of
    Just msg -> broadcastMsg sock (BL.toStrict $ encodeMsg msg) seen
    Nothing -> pure ()

-- with a snapshot of the peer socket addresses as a set, write to each peer
broadcastMsg :: Socket -> B.ByteString -> S.Set SockAddr -> IO ()
broadcastMsg sock msgData seen =
  unless (S.null seen) $
  case S.deleteFindMax seen of
    (current, rest) ->
      sendTo sock msgData current >>
      broadcastMsg sock msgData rest
