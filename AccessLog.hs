-- parse standard squid logfiles 
-- 1330757555.727  43378 144.32.71.165 TCP_MISS/200 418 GET http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041 - DIRECT/69.171.227.51 text/plain

{-# LANGUAGE BangPatterns #-}
 --{-# LANGUAGE OverloadedStrings #-}

module AccessLog
    (
      AccessLogLine,
      accessLineParser,
      ts,
      bytes,
      clientIP,
      url,
      connType,
      serverIP,
      mimeType
    ) where


import Control.Applicative
import qualified Data.Attoparsec.Lazy as A
import Data.Attoparsec.Char8 hiding (space, take)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL

import AJCUtils

data AccessLogLine = AccessLogLine {
    ts          :: !S.ByteString,
    bytes       :: !S.ByteString,
    clientIP    :: !S.ByteString,
    cacheStatus :: !S.ByteString,
    url         :: !S.ByteString,
    connType    :: !S.ByteString,  -- DIRECT etc
    serverIP    :: !S.ByteString,
    mimeType    :: !S.ByteString
} deriving (Show, Ord, Eq)

space  = satisfy (== ' ')


accessLineParser::Parser AccessLogLine
accessLineParser = do
    return $ AccessLogLine (S.pack "1") (S.pack "2") (S.pack "3") (S.pack "4") (S.pack "5") (S.pack "6")
                           (S.pack "7") (S.pack "8")
