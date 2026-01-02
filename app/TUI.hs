{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TUI (
  -- * Core Types
  ServerState (..),
  ServerEvent (..),

  -- * Lifecycle
  initTUI,
  runTUI,
  updateTUI,
  peers, -- Export peers accessor for Main.hs compatibility
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (
  TChan,
  TVar,
  atomically,
  newTChanIO,
  newTVarIO,
  readTChan,
  readTVar,
  writeTChan,
  writeTVar,
 )
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isControl, isPrint)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Network.Socket (SockAddr)

-- Brick Imports
import Brick (
  App (..),
  AttrName,
  BrickEvent (..),
  EventM,
  Padding (Max),
  ViewportType (Vertical),
  Widget,
  attrMap,
  attrName,
  customMain,
  fill,
  get,
  hBox,
  hLimitPercent,
  halt,
  modify,
  padRight,
  put,
  showFirstCursor,
  str,
  txt,
  vBox,
  vLimit,
  viewport,
  withAttr,
  zoom,
  (<+>),
 )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (defAttr, withForeColor)
import Graphics.Vty.CrossPlatform (mkVty)

-- | Resource Names (IDs for UI elements)
data Name = InputField | ChatViewport | PeerListViewport
  deriving (Ord, Show, Eq)

-- | Log levels for categorizing messages
data LogLevel
  = -- | General information (cyan) - ServerStarted, MessageReceived
    LogInfo
  | -- | Success events (green) - PeerConnected, MessageBroadcast
    LogSuccess
  | -- | Errors (red) - ParseError
    LogError
  deriving (Eq, Show)

-- | Attribute names for log levels
logInfoAttr, logSuccessAttr, logErrorAttr :: AttrName
logInfoAttr = attrName "logInfo"
logSuccessAttr = attrName "logSuccess"
logErrorAttr = attrName "logError"

-- | A log entry with level and message
data LogEntry = LogEntry
  { logLevel :: LogLevel
  , logText :: Text
  }

data ServerState = ServerState
  { _logMessages :: [LogEntry]
  -- ^ Chat history with log levels
  , _peersSet :: S.Set SockAddr
  -- ^ Connected peers (pure state)
  , _editor :: E.Editor Text Name
  -- ^ The input field
  , _peersTVar :: TVar (S.Set SockAddr)
  -- ^ Compatibility: Main.hs needs this
  , _peerMessages :: M.Map SockAddr (S.Set Integer)
  -- ^ Map of peer addresses to sets of destination (\"to\") IDs observed from them
  }

makeLenses ''ServerState

-- | Compatibility accessor for Main.hs
peers :: ServerState -> TVar (S.Set SockAddr)
peers = _peersTVar

-- | Custom Events that drive the UI
data ServerEvent
  = ServerStarted String
  | PeerConnected SockAddr
  | -- | MessageReceived sender messageId length
    MessageReceived SockAddr Integer Int
  | -- | MessageBroadcast sender to msg count
    MessageBroadcast SockAddr Integer String Int
  | ParseError String Int
  | ServerStopped

-- | Pure function: State -> [Widget]
drawUI :: ServerState -> [Widget Name]
drawUI st = [ui]
 where
  ui =
    C.center $ -- Centers the box on screen
      B.borderWithLabel (str " UDP Chat Server ") $
        padRight Max $
          hBox
            [ hLimitPercent 80 $
                vBox
                  [ renderPeers (st ^. peersSet)
                  , B.hBorder
                  , renderLog (st ^. logMessages)
                  , B.hBorder
                  , renderInput (st ^. editor)
                  ]
            , B.vBorder
            , renderPeerList (st ^. peerMessages)
            ]

-- | Sanitize text for display: remove newlines and control characters
sanitizeLogText :: Text -> Text
sanitizeLogText = T.unwords . T.words . T.map replaceControl
 where
  replaceControl c
    | c == '\n' || c == '\r' = ' '
    | isControl c = ' '
    | otherwise = c

-- | Get the prefix symbol for a log level
logPrefix :: LogLevel -> Text
logPrefix LogInfo = "[i]"
logPrefix LogSuccess = "[+]"
logPrefix LogError = "[!]"

-- | Get the attribute name for a log level
logLevelAttr :: LogLevel -> AttrName
logLevelAttr LogInfo = logInfoAttr
logLevelAttr LogSuccess = logSuccessAttr
logLevelAttr LogError = logErrorAttr

-- | Format a log entry with prefix and color
formatLogEntry :: LogEntry -> Widget Name
formatLogEntry (LogEntry level text) =
  withAttr (logLevelAttr level) $
    txt (logPrefix level <> " " <> text)

renderPeers :: S.Set SockAddr -> Widget Name
renderPeers p =
  vLimit 1 $
    str ("Connected Peers: " ++ show (S.size p))
      <+> fill ' ' -- Force width to max to clear garbage

renderLog :: [LogEntry] -> Widget Name
renderLog logs =
  -- No vLimit - let it expand to fill remaining space
  viewport ChatViewport Vertical $
    vBox $
      -- Use padRight Max to pad each line to full width
      map (padRight Max . formatLogEntry) (reverse logs)

renderInput :: E.Editor Text Name -> Widget Name
renderInput e =
  vLimit 1 $
    -- Use 'intercalate' instead of 'unlines' to prevent extra newlines
    -- Pad with fill ' ' to keep the border stable
    (str "Broadcast: " <+> E.renderEditor (txt . T.intercalate " ") True e)
      <+> fill ' '

-- | Render the peer list with destination (\"to\") IDs
renderPeerList :: M.Map SockAddr (S.Set Integer) -> Widget Name
renderPeerList peerMap =
  padRight Max $
    B.borderWithLabel (str " Peers & Messages ") $
      viewport PeerListViewport Vertical $
        vBox $
          if M.null peerMap
            then [padRight Max $ txt "No peers yet"]
            else map renderPeerEntry (M.toList peerMap)
 where
  renderPeerEntry :: (SockAddr, S.Set Integer) -> Widget Name
  renderPeerEntry (addr, msgIds) =
    vBox
      [ padRight Max $ txt (T.pack (show addr))
      , padRight Max $ txt ("  To IDs: " <> T.pack (show (S.toList msgIds)))
      , padRight Max $ txt " " -- Empty line spacer
      ]

-- | Handle both Vty events (Keyboard) and App events (Network)
appEvent :: (String -> IO ()) -> BrickEvent Name ServerEvent -> EventM Name ServerState ()
appEvent sendCallback ev = case ev of
  -- A. Handle Network Events (From your Net.hs)
  AppEvent sev -> case sev of
    ServerStarted addr ->
      addLog LogInfo $ "Server listening on " <> T.pack addr
    PeerConnected peer -> do
      -- Update both pure state and TVar for compatibility
      modify $ \s ->
        s
          & peersSet %~ S.insert peer
          & logMessages %~ (LogEntry LogSuccess (sanitizeLogText ("New peer: " <> T.pack (show peer))) :)
      -- Also update the TVar for Main.hs compatibility
      st <- get
      liftIO $ atomically $ do
        current <- readTVar (st ^. peersTVar)
        writeTVar (st ^. peersTVar) (S.insert peer current)
    MessageReceived peer msgId len -> do
      addLog LogInfo $ T.pack (show peer) <> " sent " <> T.pack (show len) <> " bytes"
      -- Track the message ID for this peer
      modify $ \s ->
        s
          & peerMessages %~ M.insertWith S.union peer (S.singleton msgId)
    MessageBroadcast sender toId msg count ->
      let toStr = if toId == 0 then "all" else T.pack (show toId)
          msgDisplay = sanitizeLogText (T.pack msg)
       in addLog LogSuccess $
            T.pack (show sender)
              <> " -> "
              <> toStr
              <> ": "
              <> msgDisplay
              <> " (broadcast to "
              <> T.pack (show count)
              <> " peers)"
    ParseError err _ ->
      addLog LogError $ T.pack err
    ServerStopped ->
      halt -- Stop the UI loop

  -- B. Handle Keyboard Events (Input)
  VtyEvent vtyE -> case vtyE of
    V.EvKey V.KEsc [] -> halt
    V.EvKey V.KEnter [] -> do
      st <- get
      let inputs = E.getEditContents (st ^. editor)
      -- Join lines safely
      let msg = T.unpack $ T.intercalate " " inputs

      liftIO $ sendCallback msg
      put $ st & editor .~ E.editor InputField (Just 1) ""

    -- Pass other keys to the editor widget
    _ -> zoom editor $ E.handleEditorEvent (VtyEvent vtyE) -- Fixed type wrapper
  _ -> return ()
 where
  addLog level text = modify $ logMessages %~ (LogEntry level (sanitizeLogText text) :)

initTUI :: IO (ServerState, TChan ServerEvent)
initTUI = do
  chan <- newTChanIO
  peersTVar <- newTVarIO S.empty
  let emptyState =
        ServerState
          { _logMessages = []
          , _peersSet = S.empty
          , _editor = E.editor InputField (Just 1) ""
          , _peersTVar = peersTVar
          , _peerMessages = M.empty
          }
  return (emptyState, chan)

-- | Helper for Net.hs to push events
updateTUI :: TChan ServerEvent -> ServerEvent -> IO ()
updateTUI chan evt = atomically $ writeTChan chan evt

{- | The Main Loop
We take the generic TChan and the Network Callback
-}
runTUI :: ServerState -> TChan ServerEvent -> (String -> IO ()) -> IO ()
runTUI initialState tChan sendCallback = do
  -- 1. Create Brick's internal channel
  bChan <- newBChan 10

  -- 2. Spawn the Bridge Thread (TChan -> BChan)
  void $ forkIO $ forever $ do
    evt <- atomically $ readTChan tChan
    writeBChan bChan evt

  -- 3. Configure the App
  let app =
        App
          { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = appEvent sendCallback
          , appStartEvent = return ()
          , appAttrMap =
              const $
                attrMap
                  V.defAttr
                  [ (logInfoAttr, V.defAttr `withForeColor` V.cyan)
                  , (logSuccessAttr, V.defAttr `withForeColor` V.green)
                  , (logErrorAttr, V.defAttr `withForeColor` V.red)
                  ]
          }

  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty

  -- Corrected customMain call:
  -- 1. initialVty (the handle)
  -- 2. buildVty (the IO action to rebuild on resume)
  void $ customMain initialVty buildVty (Just bChan) app initialState
