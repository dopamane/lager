{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lager

main :: IO ()
main = withLager "APP" [ConsoleRGB Debug] $ \l -> do
  logNotice l "Hello World!"
  logDebug l "Invisible"
  logErr l "NOO"
  logInfo l "YES"
  logWarning  l "HI"

  let l' = logSub "SUB" l
  logInfo l' "SUB!"
