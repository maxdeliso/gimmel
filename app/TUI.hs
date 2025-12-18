{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module TUI
  ( ServerState(..)
  , ServerEvent(..)
  , initTUI
  , runTUI
  , updateTUI
  , shutdownTUI
  ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
    ( atomically, newTChanIO, readTChan, writeTChan, TChan, TVar, readTVar, readTVarIO, writeTVar, newTVarIO )
import Control.Monad ( forever, when, unless )
import qualified Data.Set as S
import Network.Socket ( SockAddr )
import Graphics.Vty
    ( Vty, defaultConfig, update, picForImage, string
    , withStyle, defAttr, bold, withForeColor
    , green, red, blue, yellow, magenta, cyan, white, black
    , emptyImage, Image, (<->), (<|>)
    , nextEvent, Event(EvKey, EvResize), Key(KChar, KBS, KEnter), Modifier(MCtrl)
    )
import Graphics.Vty.CrossPlatform ( mkVty )
import qualified Graphics.Vty as V

-- Terminal-based TUI using vty library
-- Provides a clean, colorful interface with proper terminal handling

data ServerEvent
  = PeerConnected SockAddr
  | MessageReceived SockAddr Int  -- address, bytes
  | MessageBroadcast Int  -- number of peers
  | ParseError String Int  -- error message, bytes
  | ServerStarted String  -- address
  | ServerStopped
  deriving (Show)

-- Unified event type for event-driven architecture
data AppEvent
  = NetEvent ServerEvent      -- From UDP network layer
  | UIEvent V.Event           -- From Keyboard/Resize

data ServerState = ServerState
  { peers :: TVar (S.Set SockAddr)
  , messagesReceived :: TVar Int
  , messagesBroadcast :: TVar Int
  , parseErrors :: TVar Int
  , serverAddress :: TVar (Maybe String)
  , isRunning :: TVar Bool
  , vtyHandle :: TVar (Maybe Vty)
  , terminalWidth :: TVar Int  -- Dynamic terminal width for resize handling
  , inputBuffer :: TVar String  -- Current text input buffer
  }

initTUI :: IO (ServerState, TChan ServerEvent)
initTUI = do
  peers <- newTVarIO S.empty
  messagesReceived <- newTVarIO 0
  messagesBroadcast <- newTVarIO 0
  parseErrors <- newTVarIO 0
  serverAddress <- newTVarIO Nothing
  isRunning <- newTVarIO True
  vtyHandle <- newTVarIO Nothing
  terminalWidth <- newTVarIO 60  -- Default width, will be updated on resize
  inputBuffer <- newTVarIO ""
  eventChan <- newTChanIO

  let state = ServerState
        { peers = peers
        , messagesReceived = messagesReceived
        , messagesBroadcast = messagesBroadcast
        , parseErrors = parseErrors
        , serverAddress = serverAddress
        , isRunning = isRunning
        , vtyHandle = vtyHandle
        , terminalWidth = terminalWidth
        , inputBuffer = inputBuffer
        }

  return (state, eventChan)

updateTUI :: TChan ServerEvent -> ServerEvent -> IO ()
updateTUI chan event = atomically $ writeTChan chan event

runTUI :: ServerState -> TChan ServerEvent -> (String -> IO ()) -> IO ()
runTUI state netChan sendMsgFunc = do
  -- 1. Initialize Vty synchronously (No Waiting!)
  vty <- mkVty defaultConfig
  atomically $ writeTVar (vtyHandle state) (Just vty)

  -- Get initial terminal size
  display <- V.displayBounds (V.outputIface vty)
  let (w, _) = display
      initialWidth = max 60 (min w 120)  -- Clamp between 60 and 120
  atomically $ writeTVar (terminalWidth state) initialWidth

  -- 2. Create unified event channel
  appChan <- newTChanIO

  -- 3. Fork a Vty Input Bridge
  -- This thread sleeps inside Vty until a key is pressed, then wakes up.
  _ <- forkIO $ forever $ do
    evt <- nextEvent vty
    atomically $ writeTChan appChan (UIEvent evt)

  -- 4. Fork a Network Bridge
  -- Bridge network events into the unified channel
  _ <- forkIO $ forever $ do
    evt <- atomically $ readTChan netChan
    atomically $ writeTChan appChan (NetEvent evt)

  -- 5. The Tickless Loop
  let loop = do
        -- BLOCK here until *something* happens. Zero CPU usage while idle.
        evt <- atomically $ readTChan appChan

        shouldStop <- case evt of
          UIEvent (EvKey (KChar 'q') []) -> return True
          UIEvent (EvKey (KChar 'c') [MCtrl]) -> return True

          UIEvent (EvResize w _) -> do
            -- Handle terminal resize dynamically
            atomically $ writeTVar (terminalWidth state) (max 60 (min w 120))
            return False

          -- Handle text input
          UIEvent (EvKey (KChar c) []) | c /= 'q' -> do
            -- Add character to input buffer (max 256 chars)
            atomically $ do
              buf <- readTVar (inputBuffer state)
              let maxLen = 256
              when (length buf < maxLen) $ do
                writeTVar (inputBuffer state) (buf ++ [c])
            return False

          UIEvent (EvKey KBS []) -> do
            -- Backspace: remove last character
            atomically $ do
              buf <- readTVar (inputBuffer state)
              writeTVar (inputBuffer state) (if null buf then buf else init buf)
            return False

          UIEvent (EvKey KEnter []) -> do
            -- Enter: send message and clear buffer
            buf <- readTVarIO (inputBuffer state)
            unless (null buf) $ do
              sendMsgFunc buf
              atomically $ writeTVar (inputBuffer state) ""
            return False

          UIEvent _ -> return False -- Handle other keys (currently ignored)

          NetEvent serverEvt -> do
            case serverEvt of
              -- If the signal handler (or network) says stop, break the loop
              ServerStopped -> do
                handleEvent state serverEvt  -- Update state (sets isRunning to False)
                return True
              _ -> do
                handleEvent state serverEvt
                return False

        -- Redraw ONLY if we are continuing
        if shouldStop
          then do
            atomically $ writeTVar (isRunning state) False
            V.shutdown vty
          else do
            -- Build and update image
            img <- buildUI state
            update vty (picForImage img)
            loop

  -- Initial render
  img <- buildUI state
  update vty (picForImage img)

  loop

handleEvent :: ServerState -> ServerEvent -> IO ()
handleEvent state event = case event of
  PeerConnected addr -> do
    atomically $ do
      currentPeers <- readTVar (peers state)
      writeTVar (peers state) (S.insert addr currentPeers)

  MessageReceived _ bytes -> do
    atomically $ do
      count <- readTVar (messagesReceived state)
      writeTVar (messagesReceived state) (count + 1)

  MessageBroadcast peerCount -> do
    atomically $ do
      count <- readTVar (messagesBroadcast state)
      writeTVar (messagesBroadcast state) (count + 1)

  ParseError _ bytes -> do
    atomically $ do
      count <- readTVar (parseErrors state)
      writeTVar (parseErrors state) (count + 1)

  ServerStarted addr -> do
    atomically $ writeTVar (serverAddress state) (Just addr)

  ServerStopped -> do
    atomically $ writeTVar (isRunning state) False

-- Helper to ensure content exactly fills the available width
padToRight :: Int -> Image -> Image
padToRight targetWidth content =
  let currentW = V.imageWidth content
      paddingNeeded = max 0 (targetWidth - currentW)
  in content <|> string defAttr (replicate paddingNeeded ' ')

buildUI :: ServerState -> IO Image
buildUI state = do
  addr <- readTVarIO (serverAddress state)
  peerSet <- readTVarIO (peers state)
  msgCount <- readTVarIO (messagesReceived state)
  broadcastCount <- readTVarIO (messagesBroadcast state)
  errorCount <- readTVarIO (parseErrors state)
  running <- readTVarIO (isRunning state)
  boxWidth <- readTVarIO (terminalWidth state)  -- Dynamic width from state
  inputBuf <- readTVarIO (inputBuffer state)  -- Current input text

  -- 1. Define standard inner width (Total width - 4 for borders "│ " and " │")
  let innerWidth = boxWidth - 4

  -- 2. Update boxLine to use strict sizing with matching border color
  -- "│ " is 2 chars, " │" is 2 chars. Content must be innerWidth.
  let boxLine attr content =
        string attr "│ " <|> padToRight innerWidth content <|> string attr " │"

  -- 3. Helper to create box header with correct padding
  -- Format: "┌─ Title ──┐" where dashes fill to boxWidth
  -- Total: "┌─ " (3) + title + " " (1) + dashes + "┐" (1) = boxWidth
  -- So: dashes = boxWidth - 3 - length title - 1 - 1 = boxWidth - length title - 5
  let boxHeader title attr =
        let prefix = "┌─ "
            suffix = " "
            titleLen = length title
            dashesNeeded = boxWidth - length prefix - titleLen - length suffix - 1  -- -1 for "┐"
        in string attr (prefix ++ title ++ suffix ++ replicate dashesNeeded '─' ++ "┐")

  -- 4. Helper to create box footer
  -- Format: "└" + dashes + "┘" = boxWidth
  -- So: dashes = boxWidth - 2
  let boxFooter attr =
        string attr ("└" ++ replicate (boxWidth - 2) '─' ++ "┘")

  -- Header
  let blueBoldAttr = withForeColor (withStyle defAttr bold) blue
  let headerText = "GIMMEL UDP CHAT SERVER"
  let headerPadding = (boxWidth - 2 - length headerText) `div` 2
  let header = string blueBoldAttr ("╔" ++ replicate (boxWidth - 2) '═' ++ "╗") <->
               string blueBoldAttr ("║" ++ replicate headerPadding ' ' ++ headerText ++ replicate (boxWidth - 2 - length headerText - headerPadding) ' ' ++ "║") <->
               string blueBoldAttr ("╚" ++ replicate (boxWidth - 2) '═' ++ "╝")

  -- Server Status
  let greenBoldAttr = withForeColor (withStyle defAttr bold) green
  let redBoldAttr = withForeColor (withStyle defAttr bold) red
  let statusText = case addr of
        Just a -> string defAttr "Status: " <|>
                  string greenBoldAttr "RUNNING" <|>
                  string defAttr "  Address: " <|>
                  string defAttr (take 30 a)  -- Truncate address if too long
        Nothing -> string defAttr "Status: " <|>
                   string redBoldAttr "STOPPED"

  let statusBox = boxHeader "Server Status" blueBoldAttr <->
                  boxLine blueBoldAttr statusText <->
                  boxFooter blueBoldAttr

  -- Statistics
  let cyanBoldAttr = withForeColor (withStyle defAttr bold) cyan
  let statsText = string defAttr "Received: " <|>
                  string cyanBoldAttr (show msgCount) <|>
                  string defAttr "  Broadcasts: " <|>
                  string cyanBoldAttr (show broadcastCount) <|>
                  string defAttr "  Errors: " <|>
                  string redBoldAttr (show errorCount)

  let yellowBoldAttr = withForeColor (withStyle defAttr bold) yellow
  let statsBox = boxHeader "Statistics" yellowBoldAttr <->
                 boxLine yellowBoldAttr statsText <->
                 boxFooter yellowBoldAttr

  -- Connected Peers
  let magentaBoldAttr = withForeColor (withStyle defAttr bold) magenta
  let whiteAttr = withForeColor defAttr white
  let peerCount = S.size peerSet
  let peerHeaderTitle = "Connected Peers (" ++ show peerCount ++ ")"
  let peerHeader = boxHeader peerHeaderTitle magentaBoldAttr

  let peerLines = if S.null peerSet then
        [boxLine magentaBoldAttr (string whiteAttr "No peers connected yet...")]
      else
        map (\peer -> let peerStr = show peer
                          -- Truncate peer address to fit in innerWidth (accounting for "• " prefix)
                          maxPeerLen = innerWidth - 2  -- -2 for "• " prefix
                          displayStr = take maxPeerLen peerStr
                          peerText = string defAttr "• " <|> string defAttr displayStr
                      in boxLine magentaBoldAttr peerText) (S.toList peerSet)

  let peersBox = peerHeader <->
                 foldl (<->) emptyImage peerLines <->
                 boxFooter magentaBoldAttr

  -- Input Box
  let inputPrompt = string defAttr "Message: "
  -- Maximum message length is 256 characters
  let maxMessageLen = 256
  -- Truncate input buffer to fit in innerWidth (accounting for prompt)
  let promptLen = 9  -- Length of "Message: "
  let maxDisplayLen = innerWidth - promptLen
  let displayInput = take maxDisplayLen inputBuf
  let inputText = inputPrompt <|> string cyanBoldAttr displayInput
  -- Show cursor position indicator if text is truncated for display
  let displayIndicator = if length inputBuf > maxDisplayLen then "..." else ""
  -- Show warning if approaching or at message limit
  let msgLen = length inputBuf
  let limitIndicator
        | msgLen >= maxMessageLen = string redBoldAttr (" [MAX: " ++ show maxMessageLen ++ "]")
        | msgLen > maxMessageLen - 10 = string yellowBoldAttr (" [" ++ show msgLen ++ "/" ++ show maxMessageLen ++ "]")
        | otherwise = emptyImage
  let inputContent = inputText <|> string defAttr displayIndicator <|> limitIndicator

  let inputBox = boxHeader "Send Message" cyanBoldAttr <->
                 boxLine cyanBoldAttr inputContent <->
                 boxFooter cyanBoldAttr

  -- Footer
  let footerText = string defAttr "Press 'q' to quit, Enter to send, Ctrl+C to shutdown"
  let footer = string defAttr ("┌" ++ replicate (boxWidth - 2) '─' ++ "┐") <->
               boxLine defAttr footerText <->
               string defAttr ("└" ++ replicate (boxWidth - 2) '─' ++ "┘")

  return $ header <-> string defAttr "" <-> statusBox <-> string defAttr "" <-> statsBox <-> string defAttr "" <-> peersBox <-> string defAttr "" <-> inputBox <-> string defAttr "" <-> footer

shutdownTUI :: ServerState -> IO ()
shutdownTUI state = do
  atomically $ writeTVar (isRunning state) False
  maybeVty <- readTVarIO (vtyHandle state)
  case maybeVty of
    Just vty -> do
      shutdownMessage <- buildShutdownMessage
      update vty (picForImage shutdownMessage)
      -- Note: V.shutdown will be called by runTUI's loop when it exits
      -- This function just sets the flag
    Nothing -> return ()
  where
    buildShutdownMessage = return $
      string (withForeColor (withStyle defAttr bold) red) "Shutting down server..." <->
      string defAttr "Closing sockets..." <->
      string (withForeColor (withStyle defAttr bold) green) "Server stopped gracefully."
