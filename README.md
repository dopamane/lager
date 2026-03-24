# Lager :beer:

Concurrent Logger

```hs
import Lager

main = do
  l <- newLager "APP" [Console Debug]
  race (runLager l) $ do
    logDebug   l "Hello World!"
    logWarning l "Warning!"
```

Build
```
cabal build
cabal haddock
```
