{-# LANGUAGE RankNTypes #-}

module Data.Conduit.Merge (mergeSources) where

import Control.Monad.Trans (lift)
import Data.Conduit        (Producer, Source, await, newResumableSource, yield, ($$++))
import Data.List           (sortOn)

-- | Merge multiple sorted source into one sorted producer.
mergeSources :: (Ord a, Monad m) => [Source m a] -> Producer m a
mergeSources = mergeResumable . map newResumableSource
  where
    mergeResumable sources = do
        prefetchedSources <- lift $ mapM ($$++ await) sources
        go [(a, s) | (s, Just a) <- prefetchedSources]
    go [] = pure ()
    go sources = do
        let (a, src1) : sources1 = sortOn fst sources
        yield a
        (src2, mb) <- lift $ src1 $$++ await
        let sources2 = case mb of
                Nothing -> sources1
                Just b  -> (b, src2) : sources1
        go sources2
