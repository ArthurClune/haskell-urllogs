
--import qualified Codec.Compression.GZip as GZip
--import qualified Data.ByteString.Lazy.Char8 as SL
import qualified Data.ByteString.Char8 as S
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text as A

import IpoqueLog
import AccessLog
--import AJCUtils

main::IO()
main = do         
    --contents <- fmap GZip.decompress (SL.readFile "/home/arthur/Work/data/url.log.1.gz")
    --mapM_ (print . A.maybeResult . A.parse ipoqueLineParser . decodeUtf8 . toStrict) (SL.lines contents)
    contents <- (S.readFile "/home/arthur/Work/data/sample_acccess_log")
    print (A.maybeResult $ (A.parse accessLogParser $ decodeUtf8 contents))
    contents2 <- (S.readFile "/home/arthur/Work/data/url.log")
    print (A.maybeResult $ (A.parse ipoqueLogParser $ decodeUtf8 contents2))

    