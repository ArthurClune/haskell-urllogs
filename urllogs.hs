import qualified IpoqueLog
import qualified AccessLog 

main::IO()
main = do         
    --mapM_ (print . IpoqueLog.url . A.maybeResult) (IpoqueLog.getGzipLog "/home/arthur/Work/data/url.log.1.gz")
    print =<< IpoqueLog.getGzipLog "/home/arthur/Work/data/url.log.1.gz"
    print =<< AccessLog.getGzipLog "/home/arthur/Work/data/access.log-20120304.gz"

    