---
Going through Write Yourself a Scheme in 48 Hours
---

### Not all dependencies may be specified in the cabal file

ghc-ing stuff: do it with the context of cabal, using:

```hs
cabal exec -- ghc -o <outputName> <pathToFile>
```
