
{-# LANGUAGE OverloadedStrings #-}

--import Control.Monad
import System.Environment

import qualified URLAlert.SquidLog as SquidLog
import qualified URLAlert.IpoqueLog as IpoqueLog

parseFiles :: FilePath -> FilePath -> IO (Int, Int)
parseFiles f1 f2 = do
    l1 <- IpoqueLog.parseGZipLog f1::IO [Maybe IpoqueLog.IpoqueLogLine]
    l2 <- SquidLog.parseGZipLog  f2::IO [Maybe SquidLog.SquidLogLine]
    let len1 = length l1
    let len2 = length l2
    return (len1, len2)

main::IO()
main = do      
    args <- getArgs
    (len1, len2) <- parseFiles (head args) (last args)
    print $ "file1 has length " ++ show len1 ++ " and file2 has length " ++ show len2
