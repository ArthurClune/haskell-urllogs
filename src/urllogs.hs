
{-# LANGUAGE OverloadedStrings #-}

--import Control.Monad
import System.Environment
import Safe

--import qualified URLAlert.SquidLog as SquidLog
import qualified URLAlert.IpoqueLog as IpoqueLog

-- | Apply function f to a list of list of lines ls 
-- combining the results with g
analyseFiles :: (a -> b) -> ([b] -> c) -> [a] -> c
analyseFiles f g ls = g $ map f ls

parseArgs::IO [FilePath]
parseArgs = do
    args <- getArgs
    if length args > 0
        then return args
        else abort "Usage: urllogs.hs [file1] <file2> ...."

main::IO()
main = do      
    files <- parseArgs
    logLines <- (mapM IpoqueLog.parseGZipLog files)::IO [[Maybe IpoqueLog.IpoqueLogLine]]
    let len1 = analyseFiles length (foldl (+) 0) logLines
    print $ "files have " ++ show len1 ++ " log lines"
