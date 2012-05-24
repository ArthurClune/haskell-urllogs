{-# LANGUAGE OverloadedStrings #-}

import qualified Codec.Compression.GZip as GZip
import Data.ByteString.Char8 (readInt)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as SL
import qualified Data.Attoparsec as A

import IpoqueLog

-- news.bbcimg.co.uk", url = "/view/2_0_11/cream/hi/shared/components/components.css

getInt    p l = fst. fromJust . readInt $ p l
getString p l = fst. fromJust $ p l


main = do         
    contents <- fmap GZip.decompress (SL.readFile "/home/arthur/Work/data/url.log.1.gz")
    mapM_ (print . A.maybeResult . A.parse ipoqueLineParser . toStrict) (SL.lines contents)
