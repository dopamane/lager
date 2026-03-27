# Concurrent Lager :beer:

```hs
import Lager

main =
  withLager "APP" [Console Info defLevelRGB] $ \l -> do
    logDebug   l "Cheers! 🍻"
    logWarning l "Warning!"
```

Build & Docs
```
cabal build
cabal haddock
```
