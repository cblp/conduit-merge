-- stack runhaskell --package=conduit-extra --package=conduit-merge
{-# OPTIONS -Wall -Werror #-}

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8        as BS
import           Data.Conduit                 (runConduit, (.|))
import           Data.Conduit.Binary          (sinkHandle, sourceFile)
import qualified Data.Conduit.Binary          as Conduit
import qualified Data.Conduit.List            as Conduit
import           Data.Conduit.Merge           (mergeSources)
import           System.Environment           (getArgs)
import           System.IO                    (stdout)

main :: IO ()
main = do
    inputFileNames <- getArgs
    let inputs = [sourceFile file .| Conduit.lines | file <- inputFileNames]
    runResourceT $ runConduit $ mergeSources inputs .| sinkStdoutLn
  where
    sinkStdoutLn = Conduit.map (`BS.snoc` '\n') .| sinkHandle stdout
