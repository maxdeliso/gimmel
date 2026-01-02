{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module MonadNet (
  MonadNet (..),
) where

import Control.Exception (IOException)
import qualified Data.ByteString as B
import Data.Kind (Type)
import Network.Socket (AddrInfo, SockAddr)

{- | The capability to perform network operations
This typeclass abstracts over socket operations, allowing both
real IO-based implementations and pure mock implementations for testing.
-}
class (Monad m) => MonadNet m where
  -- | Abstract generic socket type (Real=Socket, Mock=Int)
  type NetSocket m :: Type

  -- | Create a socket from address information
  createSocket :: AddrInfo -> m (NetSocket m)

  -- | Bind a socket to an address
  bindSocket :: NetSocket m -> SockAddr -> m ()

  -- | Close a socket
  closeSocket :: NetSocket m -> m ()

  -- | Receive data from a socket
  receiveFrom :: NetSocket m -> Int -> m (B.ByteString, SockAddr)

  -- | Send data to a peer
  sendToPeer :: NetSocket m -> B.ByteString -> SockAddr -> m ()
