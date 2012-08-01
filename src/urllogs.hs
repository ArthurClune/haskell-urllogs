
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

--import Control.Monad
import System.Environment

import qualified URLAlert.SquidLog as SquidLog
import qualified URLAlert.IpoqueLog as IpoqueLog

-- | Apply function f to each parsed "line" from a  file 
-- given by the parser p, returning the result
--analyseFile :: forall t (m :: * -> *) b. Monad m => (t -> b) -> m t -> m b
analyseFile :: forall b c . (forall a . [a] -> b) -> IO [c] -> IO b
analyseFile f l = do
    ls <- l
    let v1 = f ls
    return v1

main::IO()
main = do      
    args <- getArgs
    len1 <- analyseFile length (IpoqueLog.parseGZipLog $ head args::IO [Maybe IpoqueLog.IpoqueLogLine])
    len2 <- analyseFile length (SquidLog.parseGZipLog  $ last args::IO [Maybe SquidLog.SquidLogLine])
    print $ "file1 has length " ++ show len1 ++ " and file2 has length " ++ show len2
