# conduit-merge

Merge multiple conduits into one.

```haskell
module Data.Conduit.Merge where

-- | Merge multiple sorted sources into one sorted producer.
mergeSources :: (Ord a, Foldable f, Monad m) => f (ConduitT () a m ()) -> ConduitT i a m ()

-- | Merge multiple sorted sources into one sorted producer using specified sorting key.
mergeSourcesOn :: (Ord b, Foldable f, Monad m) => (a -> b) -> f (ConduitT () a m ()) -> ConduitT i a m ()
```

## Example

```haskell
-- see `example.hs` for the whole program
main = do
    inputFileNames <- getArgs
    let inputs = [sourceFile file =$= Conduit.lines | file <- inputFileNames]
    runResourceT $ mergeSources inputs $$ sinkStdoutLn
```
