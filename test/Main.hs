{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async
import Lager

main :: IO ()
main = do
  l <- newLager "APP" [ConsoleRGB Debug]
  race_ (runLager l) $ do
    logNotice l "Hello World!"
    logDebug l "Invisible"
    logErr l "NOO"
    logInfo l "YES"
    logWarning  l "HI"

    let l' = logSub "SUB" l
    logInfo l' "SUB!"
