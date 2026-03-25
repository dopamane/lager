# Concurrent Lager :beer:

```hs
import Lager

main = do
  l <- newLager "APP" [Console Debug, File Info "log.txt"]
  race_ (runLager l) $ do
    logDebug   l "Hello World!"
    logWarning l "Warning!"
```

Build & Docs
```
cabal build
cabal haddock
```
