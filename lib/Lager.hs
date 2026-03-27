{-# LANGUAGE DeriveGeneric #-}

-- | Concurrent Lager 🍺
--
-- @
-- import Lager
--
-- main =
--   'withLager' \"APP\" ['File' 'Info' \"log.txt\"] $ \\l -> do
--     'logDebug' l "Cheers! 🍻"
--     'logWarn'  l "Warning!"
-- @
module Lager
  ( -- * Logging
    Lager
  , withLager
  , newLager
  , newLagerSTM
  , runLager
  , drinkLager
  , streamLager
  , extendLager
  , logDebug
  , logDebugSTM
  , logInfo
  , logInfoSTM
  , logNotice
  , logNoticeSTM
  , logWarn
  , logWarnSTM
  , logErr
  , logErrSTM
  , logCrit
  , logCritSTM
  , logAlert
  , logAlertSTM
  , logEmerg
  , logEmergSTM
  , Msg(..)
  , -- * Target
    Target(..)
  , defConsole
  , Level(..)
  , defLevelColor
  , Color(..)
  , -- * Exception
    LagerException(..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Functor
import Data.Ord
import qualified Data.Text.Lazy as T
import GHC.Generics
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.IO

-- | Logging handle
data Lager = Lager
  { nm :: String      -- source name
  , tg :: [Target]
  , wc :: TChan Msg   -- write chan
  , rc :: [TChan Msg] -- read chans
  , drink :: TVar Bool
  , drunk :: TVar Bool
  }

-- | Log message
data Msg = Msg
  { lvl :: Level
  , txt :: String -- ^ message
  , src :: String -- ^ logger source
  } deriving (Eq, Generic, Read, Show)

-- | Acquire a new handle
newLager :: String -> [Target] -> IO Lager
newLager nm' = atomically . newLagerSTM nm'

-- | Acquire a new handle in STM
newLagerSTM :: String -> [Target] -> STM Lager
newLagerSTM nm' tgt' = do
  wc' <- newBroadcastTChan
  Lager nm' tgt' wc'
    <$> replicateM (length tgt') (dupTChan wc')
    <*> newTVar False
    <*> newTVar False

-- | Acquire a 'newLager', concurrently 'runLager',
-- then finally 'drinkLager' if the body terminates
-- or throws an exception.
withLager :: String -> [Target] -> (Lager -> IO a) -> IO a
withLager nm' tgt' k = do
  l <- newLager nm' tgt'
  either id id
    <$> race (k l `finally` drinkLager l)
             (runLager l >> forever (threadDelay maxBound))

-- | Finish logging. Blocks until all outputs are written by 'runLager'.
drinkLager :: Lager -> IO ()
drinkLager l = do
  atomically $ writeTVar (drink l) True
  atomically $ checkDrunk l

checkDrink :: Lager -> STM ()
checkDrink = check <=< readTVar . drink

checkDrunk :: Lager -> STM ()
checkDrunk = check <=< readTVar . drunk

-- | Stream log messages
streamLager :: Lager -> (IO Msg -> IO a) -> IO a
streamLager l k = do
  r <- atomically $ throwIfDrunk l *> dupTChan (wc l)
  k  $ atomically $ throwIfDrunk l *> readTChan  r

-- | Extend the logger name
extendLager :: String -> Lager -> Lager
extendLager nm' l = Lager nm'' [] (wc l) [] (drink l) (drunk l)
  where
    nm'' | null nm'    = nm l
         | null (nm l) = nm'
         | otherwise   = nm l <> "|" <> nm'

-- | Log text in IO
lager :: Lager -> Level -> String -> IO ()
lager l lvl' = atomically . lagerSTM l lvl'

-- | Log text in STM
lagerSTM :: Lager -> Level -> String -> STM ()
lagerSTM l lvl' msg = do
  throwIfDrunk l
  writeTChan (wc l) $ Msg lvl' msg (nm l)

logDebug :: Lager -> String -> IO ()
logDebug l = lager l Debug

logDebugSTM :: Lager -> String -> STM ()
logDebugSTM l = lagerSTM l Debug

logInfo :: Lager -> String -> IO ()
logInfo l = lager l Info

logInfoSTM :: Lager -> String -> STM ()
logInfoSTM l = lagerSTM l Info

logNotice :: Lager -> String -> IO ()
logNotice l = lager l Notice

logNoticeSTM :: Lager -> String -> STM ()
logNoticeSTM l = lagerSTM l Notice

logWarn :: Lager -> String -> IO ()
logWarn l = lager l Warn

logWarnSTM :: Lager -> String -> STM ()
logWarnSTM l = lagerSTM l Warn

logErr :: Lager -> String -> IO ()
logErr l = lager l Err

logErrSTM :: Lager -> String -> STM ()
logErrSTM l = lagerSTM l Err

logCrit :: Lager -> String -> IO ()
logCrit l = lager l Crit

logCritSTM :: Lager -> String -> STM ()
logCritSTM l = lagerSTM l Crit

logAlert :: Lager -> String -> IO ()
logAlert l = lager l Alert

logAlertSTM :: Lager -> String -> STM ()
logAlertSTM l = lagerSTM l Alert

logEmerg :: Lager -> String -> IO ()
logEmerg l = lager l Emerg

logEmergSTM :: Lager -> String -> STM ()
logEmergSTM l = lagerSTM l Emerg

-- | Log level
data Level
  = Emerg -- ^ emergency
  | Alert
  | Crit  -- ^ critcal
  | Err   -- ^ error
  | Warn  -- ^ warning
  | Notice
  | Info
  | Debug
  deriving (Enum, Eq, Generic, Read, Show)

-- | 'Debug' < 'Emerg'
instance Ord Level where
  compare = comparing $ negate . fromEnum

annLevelColor :: Level -> [(Level, Color)] -> Doc AnsiStyle -> Doc AnsiStyle
annLevelColor l m = case lookup l m of
  Just c  -> annotate $ color c
  Nothing -> id

-- | Default 'Console' color schema
defLevelColor :: [(Level, Color)]
defLevelColor =
  [ (Emerg, Red), (Alert, Red), (Crit, Red), (Err, Red)
  , (Warn, Yellow), (Notice, Green), (Debug, Cyan)
  ]

-- | Log output
data Target
  = Console Level [(Level, Color)] -- ^ stdout, color schema
  | Journal Level                  -- ^ journald stdout
  | File Level FilePath
  deriving (Eq, Generic, Show)

-- | Default 'Console' target with 'Info' log level
-- and 'defLevelColor' color schema.
defConsole :: Target
defConsole = Console Info defLevelColor

-- | Run logging daemon
runLager :: Lager -> IO ()
runLager l = run `finally` atomically (writeTVar (drunk l) True)
  where
    run = case zip (tg l) (rc l) of
      [] -> atomically $ checkDrink l
      ts -> mapConcurrently_ (runTarget l) ts

runTarget :: Lager -> (Target, TChan Msg) -> IO ()
runTarget lgr (t, c) = case t of
  Console l m -> runHandle lgr stdout (renderConsoleColor m) l c
  Journal l -> runHandle lgr stdout renderJournal l c
  File l path ->
    withFile path WriteMode $ \hndl ->
      runHandle lgr hndl renderConsole l c

renderJournal :: Msg -> String
renderJournal msg =
  "<" <> show (fromEnum $ lvl msg) <> "> " <> renderConsole msg

renderConsole :: Msg -> String
renderConsole msg
  | null (src msg) = txt msg
  | otherwise      = "[" <> src msg <> "] " <> txt msg

renderConsoleColor :: [(Level, Color)] -> Msg -> String
renderConsoleColor m msg =
  T.unpack $
  renderLazy $
  layoutPretty defaultLayoutOptions $
  annLevelColor (lvl msg) m $
  pretty $
  renderConsole msg

runHandle
  :: Lager
  -> Handle
  -> (Msg -> String)
  -> Level
  -> TChan Msg
  -> IO ()
runHandle lgr hndl render l c = loop
  where
    loop = join $ atomically $ do
      m <- Just `fmap` readTChan c <|> (checkDrink lgr $> Nothing)
      case m of
        Just a  -> return $ logHandle hndl render l a >> loop
        Nothing -> do
          msgs <- listTChan c
          return $ mapM_ (logHandle hndl render l) msgs

logHandle :: Handle -> (Msg -> String) -> Level -> Msg -> IO ()
logHandle hndl render l msg
  | not (visible l msg) = return ()
  | otherwise           = hPutStrLn hndl $ render msg

listTChan :: TChan a -> STM [a]
listTChan t = loop
  where
    loop = do
      isEmpty <- isEmptyTChan t
      if isEmpty
        then return []
        else (:) <$> readTChan t <*> loop

visible :: Level -> Msg -> Bool
visible lvl' msg = lvl msg >= lvl'

data LagerException
  = LagerDaemonTerminated
    -- ^ the daemon is already closed due to 'drinkLager'
    -- or an exception

instance Show LagerException where
  show LagerDaemonTerminated = "lager: daemon terminated"

instance Exception LagerException

throwIfDrunk :: Lager -> STM ()
throwIfDrunk l = do
  isDrunk <- readTVar $ drunk l
  when isDrunk $ throwSTM LagerDaemonTerminated
