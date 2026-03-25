{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async
import Lager

main :: IO ()
main = do
  l <- newLager "APP" [File Info "log.txt"]
  race_ (runLager l) $ do
    logDebug l "Hello World!"
    logDebug l "Invisible"
    logDebug l "NOO"
    logDebug l "YES"
    logWarning  l "HI"

    let l' = logSub "SUB" l
    logInfo l' "SUB!"
