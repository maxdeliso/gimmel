{-# LANGUAGE CPP #-}

module Main where

import Net
    ( netMain, ServerResources(..), shutdownNet, sendMessage )
import Options
    ( Options, optParser )
import Options.Applicative
    ( fullDesc, info, progDesc, execParser, helper )
import TUI
    ( ServerState(..), ServerEvent(..), initTUI, runTUI, updateTUI )

import Control.Concurrent ( MVar, newMVar, readMVar, modifyMVar_ )
import Control.Concurrent.STM ( atomically, writeTChan )
import Control.Exception ( catch, AsyncException(..) )
import System.IO ( hSetBuffering, BufferMode(NoBuffering), stdin )

#if defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS)
-- Windows: Use exception handling for Ctrl+C
#else
-- Unix: Use POSIX signals
import System.Posix.Signals
    ( installHandler, Handler(..), sigTERM, sigINT )
#endif

main :: IO ()
main = do
  options <- execParser opts
  runWithOpts options
  where
    opts = info (helper <*> optParser) (fullDesc <> progDesc "UDP chat server")

runWithOpts :: Options -> IO ()
runWithOpts userOptions = do
  hSetBuffering stdin NoBuffering
  -- 1. Init State
  (tuiState, eventChan) <- initTUI

  -- Set up signal handlers
  resourcesVar <- newMVar Nothing

  -- Capture eventChan in the handler environment
  let shutdownHandler = do
        putStr "\n"  -- New line after ^C
        resources <- readMVar resourcesVar
        case resources of
          Just res -> do
            -- CLEANUP 1: Close sockets immediately (so we stop receiving)
            shutdownNet res
            -- CLEANUP 2: Wake up the TUI loop so it can exit naturally
            -- Inject ServerStopped event to unblock the main thread
            atomically $ writeTChan eventChan ServerStopped
          Nothing -> do
            -- Even if network isn't started, wake up the TUI
            atomically $ writeTChan eventChan ServerStopped
        -- Do NOT exitSuccess here. Let runTUI finish and return naturally.

#if defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS)
  -- Windows: Catch UserInterrupt exception
#else
  -- Unix: Install POSIX signal handlers
  _ <- installHandler sigINT (Catch shutdownHandler) Nothing
  _ <- installHandler sigTERM (Catch shutdownHandler) Nothing
#endif

  -- 2. Start Network (It runs in background threads defined in Net.hs)
  -- Pass the channel so Net.hs can feed the TUI
  resources <- netMain userOptions eventChan (peers tuiState)
  modifyMVar_ resourcesVar (\_ -> return $ Just resources)

  -- 3. Run TUI (Blocking call on Main Thread)
  -- When this returns, the user has quit.
#if defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS)
  -- Windows: Catch UserInterrupt exception
  catch
    (runTUI tuiState eventChan (sendMessage resources))
    (\e@UserInterrupt -> shutdownHandler)
#else
  runTUI tuiState eventChan (sendMessage resources)
#endif

  -- 4. Cleanup
  shutdownNet resources
