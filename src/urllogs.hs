
import qualified URLAlert.SquidLog as SquidLog
import qualified URLAlert.IpoqueLog as IpoqueLog

main::IO()
main = do      
    print =<< (IpoqueLog.parseGZipLog 
        "/home/arthur/Work/data/url.log.1.gz"::IO [Maybe SquidLog.SquidLogLine])
    print =<< (SquidLog.parseGZipLog 
        "/home/arthur/Work/data/access.log-20120304.gz"::IO [Maybe IpoqueLog.IpoqueLogLine])
