{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Net (
  ServerResources (..),
  netMain,
  sendMessageIO,
  shutdownNetIO,
 )
import Network.Socket (SockAddr, Socket)
import Options (
  Options (..),
  optParser,
 )
import Options.Applicative (
  execParser,
  fullDesc,
  helper,
  info,
  progDesc,
 )
import TUI (
  ServerEvent (..),
  ServerState (..),
  initTUI,
  peers,
  runTUI,
  updateTUI,
 )

-- Concurrency & Control Flow
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Async (race_)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.STM (TChan, TVar, atomically, writeTChan)
import Control.Exception (AsyncException (UserInterrupt), SomeException, bracket, try)
import Control.Monad (forever, void, when)
import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import qualified Data.Set as S

-- System
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdin)

#if !defined(mingw32_HOST_OS) && !defined(mingw32_TARGET_OS)
import System.Posix.Signals ( installHandler, Handler(Catch), sigINT, sigTERM )
#endif

{- | The Environment for the Main entry point
We only need the TUI channel and Options here.
Network resources are handled via 'bracket' so they don't need to be in the Env.
-}
data MainEnv = MainEnv
  { mainOptions :: Options
  , mainChan :: TChan ServerEvent
  }

type MainM a = ReaderT MainEnv IO a

main :: IO ()
main = do
  options <- execParser opts
  -- 1. Init TUI (Get the channel)
  (tuiState, eventChan) <- initTUI
  hSetBuffering stdin NoBuffering

  let env = MainEnv options eventChan

  -- 2. Run the Application
  runReaderT (appMain tuiState) env
 where
  opts = info (helper <*> optParser) (fullDesc <> progDesc "UDP chat server")

-- | The core application logic
appMain :: ServerState -> MainM ()
appMain tuiState = do
  env <- ask
  let options = mainOptions env
  let chan = mainChan env

  liftIO $
    bracket
      (acquireNet options chan (peers tuiState))
      releaseNet
      (runSystemLoop tuiState chan)

-- | 1. Setup Phase
acquireNet :: Options -> TChan ServerEvent -> TVar (S.Set SockAddr) -> IO (ServerResources Socket)
acquireNet opts chan peers = do
  -- We assume netMain from the previous refactor (which returns IO ServerResources)
  -- If your netMain is NetM, run it here.
  netMain opts chan peers

{- | 2. Work Phase: The "Race"
We race the TUI against the Signal Handler. Whichever finishes first wins.
-}
runSystemLoop :: ServerState -> TChan ServerEvent -> ServerResources Socket -> IO ()
runSystemLoop tuiState chan resources = do
  let runUI = runTUI tuiState chan (sendMessageIO resources)

  -- race_ runs both actions. If one finishes (or crashes), it cancels the other.
  race_
    (handleSignals chan) -- A. Wait for Ctrl+C
    runUI -- B. Run the Interface

-- | 3. Teardown Phase
releaseNet :: ServerResources Socket -> IO ()
releaseNet resources = do
  shutdownNetIO resources

-- | Blocks until a signal is received, then injects ServerStopped to TUI
handleSignals :: TChan ServerEvent -> IO ()
handleSignals chan = do
#if defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS)
    -- Windows: We just sleep forever.
    -- 'race_' will catch the UserInterrupt exception thrown to the main thread
    -- when Ctrl+C is pressed, cancelling runUI automatically.
    loopForever
#else
    -- Unix: We must explicitly catch signals and unblock the TUI
    waitVar <- MVar.newEmptyMVar
    let handler = MVar.putMVar waitVar ()

    _ <- installHandler sigINT (Catch handler) Nothing
    _ <- installHandler sigTERM (Catch handler) Nothing

    -- Block here until signal fires
    MVar.takeMVar waitVar

    -- Notify TUI to stop strictly
    atomically $ writeTChan chan ServerStopped
#endif

loopForever :: IO ()
loopForever = do
  threadDelay 10000000
  loopForever
