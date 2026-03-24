{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async
import Lager

main :: IO ()
main = do
  l <- newLager "APP" [Journal Debug]
  race_ (runLager l) $ do
    lager l Debug "Hello World!"
    lager l Debug "Invisible"
    lager l Debug "NOO"
    lager l Debug "YES"
    lager l Warning "HI"

    let l' = dupLager "SYS" l
    lager l' Info "DUPE!"
