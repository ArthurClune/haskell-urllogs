
import qualified URLAlert.AccessLog as AccessLog
import qualified URLAlert.IpoqueLog as IpoqueLog

main::IO()
main = do      
    print =<< IpoqueLog.getGzipLog "/home/arthur/Work/data/url.log.1.gz"
    print =<< AccessLog.getGzipLog "/home/arthur/Work/data/access.log-20120304.gz"
