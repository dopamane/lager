# Concurrent Lager :beer:

```hs
import Lager

main =
  withLager "APP" [ConsoleRGB Info] $ \l -> do
    logDebug   l "Cheers! 🍻"
    logWarning l "Warning!"
```

Build & Docs
```
cabal build
cabal haddock
```
