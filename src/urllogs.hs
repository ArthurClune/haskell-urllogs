
import qualified URLAlert.SquidLog as SquidLog
import qualified URLAlert.IpoqueLog as IpoqueLog

main::IO()
main = do      
    print =<< (IpoqueLog.parseGZipLog 
        "/Users/arthur/Work/data/url.log.1.gz"::IO [Maybe IpoqueLog.IpoqueLogLine])
    print =<< (SquidLog.parseGZipLog 
        "/Users/arthur/Work/data/access.log-20120621.gz"::IO [Maybe SquidLog.SquidLogLine])
