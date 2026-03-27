{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Concurrent Lager 🍺
--
-- @
-- import Lager
--
-- main =
--   'withLager' \"APP\" ['File' 'Info' \"log.txt\"] $ \\l -> do
--     'logDebug'   l "Cheers! 🍻"
--     'logWarning' l "Warning!"
-- @
module Lager
  ( Lager
  , withLager
  , newLager
  , newLagerSTM
  , runLager
  , drinkLager
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
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Functor
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
  , drink :: TVar Bool
  , drunk :: TVar Bool
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
  Lager nm' tgt' wc'
    <$> replicateM (length tgt') (dupTChan wc')
    <*> newTVar False
    <*> newTVar False

-- | Acquire a 'Lager' and ensure logs are flushed if the
-- body terminates or throws an exception.
withLager :: Text -> [Target] -> (Lager -> IO a) -> IO a
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
logSTM l lvl' msg = do
  isDrunk <- readTVar (drunk l)
  when isDrunk $ throwSTM LagerDaemonTerminated
  writeTChan (wc l) $ Msg lvl' msg (nm l)

-- | Extend the logger name
logSub :: Text -> Lager -> Lager
logSub nm' l = Lager nm'' [] (wc l) [] (drink l) (drunk l)
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
runLager :: Lager -> IO ()
runLager l =
  mapConcurrently_ id (zipWith (runTarget l) (tg l) (rc l))
    `finally` atomically (writeTVar (drunk l) True)

runTarget :: Lager -> Target -> TChan Msg -> IO ()
runTarget lgr t c = case t of
  Console l -> runHandle lgr stdout renderConsole l c
  ConsoleRGB l -> runHandle lgr stdout renderConsoleRGB l c
  Journal l -> runHandle lgr stdout renderJournal l c
  File l path ->
    withFile path WriteMode $ \hndl ->
      runHandle lgr hndl renderConsole l c

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

runHandle
  :: Lager
  -> Handle
  -> (Msg -> Text)
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

logHandle :: Handle -> (Msg -> Text) -> Level -> Msg -> IO ()
logHandle hndl render l msg
  | not (visible l msg) = return ()
  | otherwise           = T.hPutStrLn hndl $ render msg

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

instance Show LagerException where
  show LagerDaemonTerminated = "lager: daemon terminated"

instance Exception LagerException
