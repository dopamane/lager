{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Concurrent Lager 🍺
--
-- @
-- import Lager
--
-- main = do
--   l <- 'newLager' \"APP\" ['Console' 'Debug', 'File' 'Info' \"log.txt\"]
--   race_ ('runLager' l) $ do
--     'logDebug'   l "Cheers! 🍻"
--     'logWarning' l "Warning!"
-- @
module Lager
  ( Lager
  , newLager
  , newLagerSTM
  , runLager
  , logDebug
  , logInfo
  , logNotice
  , logWarning
  , logErr
  , logCrit
  , logAlert
  , logEmerg
  , logSTM
  , logStream
  , logSub
  , Msg(..)
  , Target(..)
  , Level(..)
  ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Ord
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
import GHC.Generics
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.IO

-- | Logging handle
data Lager = Lager
  { nm :: Text        -- source name
  , tg :: [Target]
  , wc :: TChan Msg   -- write chan
  , rc :: [TChan Msg] -- read chans
  }

-- | Log message
data Msg = Msg
  { lvl :: Level
  , txt :: Text -- ^ message
  , src :: Text -- ^ logger source
  }

-- | Acquire a new handle
newLager :: Text -> [Target] -> IO Lager
newLager nm' = atomically . newLagerSTM nm'

-- | Acquire a new handle in STM
newLagerSTM :: Text -> [Target] -> STM Lager
newLagerSTM nm' tgt' = do
  wc' <- newBroadcastTChan
  rc' <- replicateM (length tgt') $ dupTChan wc'
  return $ Lager nm' tgt' wc' rc'

logDebug :: Lager -> Text -> IO ()
logDebug l = lager l Debug

logInfo :: Lager -> Text -> IO ()
logInfo l = lager l Info

logNotice :: Lager -> Text -> IO ()
logNotice l = lager l Notice

logWarning :: Lager -> Text -> IO ()
logWarning l = lager l Warning

logErr :: Lager -> Text -> IO ()
logErr l = lager l Err

logCrit :: Lager -> Text -> IO ()
logCrit l = lager l Crit

logAlert :: Lager -> Text -> IO ()
logAlert l = lager l Alert

logEmerg :: Lager -> Text -> IO ()
logEmerg l = lager l Emerg

-- | Log text in IO
lager :: Lager -> Level -> Text -> IO ()
lager l lvl' = atomically . logSTM l lvl'

-- | Log text in STM
logSTM :: Lager -> Level -> Text -> STM ()
logSTM l lvl' msg = writeTChan (wc l) $ Msg lvl' msg (nm l)

-- | Extend the logger name
logSub :: Text -> Lager -> Lager
logSub nm' l = Lager nm'' [] (wc l) []
  where
    nm'' | T.null nm'    = nm l
         | T.null (nm l) = nm'
         | otherwise     = nm l <> "|" <> nm'

-- | Stream log messages
logStream :: Lager -> (IO Msg -> IO a) -> IO a
logStream l k = do
  r <- atomically $ dupTChan $ wc l
  k  $ atomically $ readTChan  r

-- | Log level
data Level
  = Emerg
  | Alert
  | Crit
  | Err
  | Warning
  | Notice
  | Info
  | Debug
  deriving (Enum, Eq, Generic, Read, Show)

instance Ord Level where
  compare = comparing $ negate . fromEnum

annLevelColor :: Level -> Doc AnsiStyle -> Doc AnsiStyle
annLevelColor l = case l of
  Emerg   -> annotate $ color Red
  Alert   -> annotate $ color Red
  Crit    -> annotate $ color Red
  Err     -> annotate $ color Red
  Warning -> annotate $ color Yellow
  Notice  -> annotate $ color Green
  Info    -> id
  Debug   -> annotate $ color Cyan

-- | Log output
data Target
  = Console Level -- ^ stdout
  | ConsoleRGB Level -- ^ stdout RGB
  | Journal Level -- ^ journald stdout
  | File Level FilePath
  deriving (Eq, Generic, Read, Show)

-- | Run logging daemon
runLager :: Lager -> IO a
runLager l = runConcurrently $ asum $ zipWith runTarget (tg l) (rc l)

runTarget :: Target -> TChan Msg -> Concurrently a
runTarget t c = Concurrently $ case t of
  Console l -> runHandle stdout renderConsole l c
  ConsoleRGB l -> runHandle stdout renderConsoleRGB l c
  Journal l -> runHandle stdout renderJournal l c
  File l path ->
    withFile path WriteMode $ \hndl ->
      runHandle hndl renderConsole l c

renderJournal :: Msg -> Text
renderJournal msg =
  "<" <> T.pack (show $ fromEnum $ lvl msg) <> "> " <> renderConsole msg

renderConsole :: Msg -> Text
renderConsole msg
  | T.null (src msg) = txt msg
  | otherwise        = "[" <> src msg <> "] " <> txt msg

renderConsoleRGB :: Msg -> Text
renderConsoleRGB msg =
  renderLazy $
  layoutPretty defaultLayoutOptions $
  annLevelColor (lvl msg) $
  pretty $
  renderConsole msg

runHandle :: Handle -> (Msg -> Text) -> Level -> TChan Msg -> IO a
runHandle hndl render l c =
  forever $ logHandle hndl render l =<< atomically (readTChan c)

logHandle :: Handle -> (Msg -> Text) -> Level -> Msg -> IO ()
logHandle hndl render l msg
  | not (visible l msg) = return ()
  | otherwise           = T.hPutStrLn hndl $ render msg

visible :: Level -> Msg -> Bool
visible lvl' msg = lvl msg >= lvl'
