
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad

import qualified URLAlert.SquidLog as SquidLog
import qualified URLAlert.IpoqueLog as IpoqueLog

main::IO()
main = do      
    --print =<< (IpoqueLog.parseGZipLog 
    --    "/Users/arthur/Work/data/url.log.1.gz"::IO [Maybe IpoqueLog.IpoqueLogLine])
    --print =<< (SquidLog.parseGZipLog 
    --    "/Users/arthur/Work/data/access.log-20120621.gz"::IO [Maybe SquidLog.SquidLogLine])

   len1 <- liftM length (IpoqueLog.parseGZipLog "/Users/arthur/Work/data/url.log.1.gz"::IO [Maybe IpoqueLog.IpoqueLogLine])
   len2 <- liftM length (SquidLog.parseGZipLog  "/Users/arthur/Work/data/access.log-20120621.gz"::IO [Maybe SquidLog.SquidLogLine])

   print $ "file1 has length" ++ (show len1) ++ " and file2 has length" ++ (show len2)
