{-# LANGUAGE RankNTypes #-}

module Data.Conduit.Merge (mergeSources, mergeSourcesOn) where

import Control.Monad.Trans (lift)
import Data.Conduit        (ConduitT, await, sealConduitT, yield, ($$++))
import Data.Foldable       (toList)
import Data.List           (sortOn)

-- | Merge multiple sorted sources into one sorted producer.
mergeSources :: (Ord a, Foldable f, Monad m) => f (ConduitT () a m ()) -> ConduitT i a m ()
mergeSources = mergeSourcesOn id
{-# INLINE mergeSources #-}

-- | Merge multiple sorted sources into one sorted producer using specified sorting key.
mergeSourcesOn
    :: (Ord b, Foldable f, Monad m)
    => (a -> b) -> f (ConduitT () a m ()) -> ConduitT i a m ()
mergeSourcesOn key = mergeSealed . fmap sealConduitT . toList where
    mergeSealed sources = do
        prefetchedSources <- lift $ traverse ($$++ await) sources
        go [(a, s) | (s, Just a) <- prefetchedSources]
    go sources =
        case sortOn (key . fst) sources of
            []                   -> pure ()
            (a, src1) : sources1 -> do
                yield a
                (src2, mb) <- lift $ src1 $$++ await
                let sources2 = case mb of
                        Nothing -> sources1
                        Just b  -> (b, src2) : sources1
                go sources2
