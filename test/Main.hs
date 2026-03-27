module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Lager

main :: IO ()
main =
  withLager "APP" [Console Debug defLevelColor] $ \l ->
  streamLager l $ \msgIO ->
  concurrently_ (streaming msgIO) $ do
    logNotice l "Hello World!"
    logDebug l "Invisible"
    logErr l "NOO"
    logInfo l "YES"
    logWarn  l "HI"
    let l' = extendLager "SUB" l
    logInfo l' "SUB!"
    forever $ do
      mapConcurrently_ id
        [ -- logEmerg l "Emergency!"
          logAlert l "Alert!"
        , logCrit l "Critical."
        , logErr l "Error."
        , logWarn l "Warning"
        , logNotice l "Notice"
        , logInfo l "Information"
        , logDebug l "Debugging"
        ]
      threadDelay 1000000
  where
    streaming msgIO = forever $ print =<< msgIO
