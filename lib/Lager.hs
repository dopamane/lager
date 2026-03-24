{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Concurrent Logger 🍺
--
-- @
-- main = do
--   l <- newLager "APP" [Console Info]
--   race (runLager l) $ do
--     logDebug l "Hello World!"
--     logInfo  l "Information!"
--     logErr   l "Error!"
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
  , lager
  , lagerSTM
  , streamLager
  , Msg(..)
  , dupLager
  , Target(..)
  , Level(..)
  ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy    as T
import qualified Data.Text.Lazy.IO as T
import GHC.Generics
import System.IO

-- | Logging handle
data Lager = Lager
  { nm :: Text
  , tg :: [Target]
  , wc :: TChan Msg
  , rc :: [TChan Msg]
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
lager l lvl' = atomically . lagerSTM l lvl'

-- | Log text in STM
lagerSTM :: Lager -> Level -> Text -> STM ()
lagerSTM l lvl' msg = writeTChan (wc l) $ Msg lvl' msg (nm l)

-- | Extend the logger name
dupLager :: Text -> Lager -> Lager
dupLager nm' l = Lager nm'' [] (wc l) []
  where
    nm'' | T.null nm'    = nm l
         | T.null (nm l) = nm'
         | otherwise     = nm l <> "|" <> nm'

-- | Stream log messages
streamLager :: Lager -> (IO Msg -> IO a) -> IO a
streamLager l k = do
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
  deriving (Enum, Eq, Generic, Ord, Read, Show)

-- | Log output
data Target
  = Console Level
  | File Level FilePath
  | Journal Level
  deriving (Eq, Generic, Read, Show)

-- | Run logging daemon
runLager :: Lager -> IO a
runLager l = runConcurrently $ asum $ zipWith runTarget (tg l) (rc l)

runTarget :: Target -> TChan Msg -> Concurrently a
runTarget t c = Concurrently $ case t of
  Console l -> runHandle stdout renderMsg l c
  File l path -> runFile l path c
  Journal l -> runJournal l c

runFile :: Level -> FilePath -> TChan Msg -> IO a
runFile l path c =
  withFile path WriteMode $ \hndl ->
    runHandle hndl renderMsg l c

runJournal :: Level -> TChan Msg -> IO a
runJournal = runHandle stdout render
  where
    render msg =
      "<" <> T.pack (show $ fromEnum $ lvl msg) <> "> " <> renderMsg msg

runHandle :: Handle -> (Msg -> Text) -> Level -> TChan Msg -> IO a
runHandle hndl render l c =
  forever $ logHandle hndl render l =<< atomically (readTChan c)

logHandle :: Handle -> (Msg -> Text) -> Level -> Msg -> IO ()
logHandle hndl render l msg
  | not (visible l msg) = return ()
  | otherwise           = T.hPutStrLn hndl $ render msg

renderMsg :: Msg -> Text
renderMsg msg
  | T.null (src msg) = txt msg
  | otherwise        = "[" <> src msg <> "] " <> txt msg

visible :: Level -> Msg -> Bool
visible lvl' msg = lvl msg <= lvl'
