{-# LANGUAGE OverloadedStrings #-}

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as SL
import qualified Data.Attoparsec as A

import IpoqueLogs

-- news.bbcimg.co.uk", url = "/view/2_0_11/cream/hi/shared/components/components.css

--matchURL::IpoqueLogLine String -> Bool
matchURL l s = (sport $ l) == "80"

main = do         
    contents <- fmap GZip.decompress (BL.readFile "/home/arthur/Work/data/url.log.1.gz")
    mapM_ (print . A.maybeResult . A.parse ipoqueLineParser . toStrict) (SL.lines contents)
