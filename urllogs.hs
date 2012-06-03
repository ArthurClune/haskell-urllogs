
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy.Char8 as SL
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text as A

import IpoqueLog

main::IO()
main = do         
    contents <- fmap GZip.decompress (SL.readFile "/home/arthur/Work/data/url.log.1.gz")
    mapM_ (print . A.maybeResult . A.parse ipoqueLineParser . decodeUtf8 . toStrict) (SL.lines contents)
