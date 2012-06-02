{-# LANGUAGE OverloadedStrings #-}

import qualified Codec.Compression.GZip as GZip
--import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as SL
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Attoparsec.Text as A

import IpoqueLog

main = do         
    contents <- fmap GZip.decompress (SL.readFile "/home/arthur/Work/data/url.log.1.gz")
    mapM_ (print . A.maybeResult . A.parse ipoqueLineParser . decodeUtf8 . toStrict) (SL.lines contents)
