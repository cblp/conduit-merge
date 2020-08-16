{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Conduit (ConduitT, lift, runConduit, yield, (.|))
import qualified Data.Conduit.Combinators as Conduit
import           Numeric.Natural (Natural)
import           System.Environment (getArgs, getExecutablePath)
import           System.Process (callProcess)
import           System.Random (randomRIO)

import           Data.Conduit.Merge (mergeSources)

main :: IO ()
main = do
  exe  <- getExecutablePath
  args <- getArgs
  case args of
    []        -> callProcess exe ["INNER", "+RTS", "-K1K", "-RTS"]
    ["INNER"] -> test
    _         -> fail "internal error"

test :: IO ()
test =
  do
    n :: Natural <-
      runConduit $
        mergeSources (replicate sourceCount $ testSource sourceSize)
        .| Conduit.length
    print n
  where
    sourceCount = 10
    sourceSize  = 100_000

testSource :: Natural -> ConduitT i Integer IO ()
testSource size =
  do
    start <- lift $ randomRIO (-100, 100)
    go start size
  where
    go _ 0 = pure ()
    go x n = do
      yield x
      d <- lift $ randomRIO (0, 10)
      go (x + d) (pred n)
