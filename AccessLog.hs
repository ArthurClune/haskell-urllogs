-- parse standard squid logfiles 
-- 1330757555.727  43378 144.32.71.165 TCP_MISS/200 418 GET http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041 - DIRECT/69.171.227.51 text/plain

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module AccessLog
    (
      AccessLogLine,
      accessLogLine,
      ts,
      elapsed,
      clientIP,
      action,
      size,
      method,
      url,
      ident,
      hierarchy,
      mimeType
    ) where


import Prelude hiding (takeWhile, take)
import Control.Applicative
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.Char8

data AccessLogLine = AccessLogLine {
    ts        :: !S.ByteString,
    elapsed   :: !S.ByteString,
    clientIP  :: !S.ByteString,
    action    :: !S.ByteString,
    size      :: !S.ByteString,
    method    :: !S.ByteString,
    url       :: !S.ByteString,
    ident     :: !S.ByteString,
    hierarchy :: !S.ByteString,
    mimeType  :: !S.ByteString
} deriving (Show, Ord, Eq)

plainValue::Parser S.ByteString
plainValue = takeWhile1 (/= ' ')
{-# INLINE plainValue #-}

accessLogLine::Parser AccessLogLine
accessLogLine = do
    lts        <- plainValue
    lelapsed   <- skipWhile (== ' ') *> plainValue
    lclientIP  <- space *> plainValue
    laction    <- space *> plainValue
    lsize      <- space *> plainValue
    lmethod    <- space *> plainValue
    lurl       <- space *> plainValue
    lident     <- space *> plainValue
    lhierarchy <- space *> plainValue
    lmimeType  <- space *> takeByteString <* endOfInput
    return $ AccessLogLine lts lelapsed lclientIP laction lsize lmethod lurl lident lhierarchy lmimeType
