
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy.Char8 as SL
import qualified Data.Attoparsec.ByteString.Lazy as A

import IpoqueLog
import AccessLog

main::IO()
main = do         
    contents <- fmap GZip.decompress (SL.readFile "/home/arthur/Work/data/url.log.1.gz") 
    mapM_ (print . A.maybeResult . A.parse ipoqueLogLine) (SL.lines contents)
    contents2 <- fmap GZip.decompress (SL.readFile "/home/arthur/Work/data/access.log-20120304.gz")
    mapM_ (print . A.maybeResult . A.parse accessLogLine) (SL.lines contents2)

    