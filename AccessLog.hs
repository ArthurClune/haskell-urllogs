-- parse standard squid logfiles 
-- 1330757555.727  43378 144.32.71.165 TCP_MISS/200 418 GET http://0-if-w.channel.facebook.com/pull?channel=p_100003547657244&seq=6&clientid=71560450&cb=f7ry&idle=46041 - DIRECT/69.171.227.51 text/plain

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module AccessLog
    (
      AccessLogLine,
      accessLineParser,
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


import Prelude hiding (takeWhile)
import Control.Applicative
import qualified Data.Text as T
import Data.Attoparsec.Text

data AccessLogLine = AccessLogLine {
    ts        :: !Number,
    elapsed   :: !Number,
    clientIP  :: !T.Text,
    action    :: !T.Text,
    size      :: Number,
    method    :: !T.Text,
    url       :: !T.Text,
    ident     :: !T.Text,  
    hierarchy :: !T.Text,
    mimeType  :: !T.Text
} deriving (Show, Ord, Eq)

value :: Parser T.Text
value = takeWhile1 (/= ' ')
{-# INLINE value #-}

accessLineParser::Parser AccessLogLine
accessLineParser = do
    lts        <- number
    lelapsed   <- skipWhile (== ' ') *> number
    lclientIP  <- space *> value
    laction    <- space *> value
    lsize      <- space *> number
    lmethod    <- space *> value
    lurl       <- space *> value
    lident     <- space *> value
    lhierarchy <- space *> value
    lmimeType  <- space *> takeTill (isEndOfLine)
    return $ AccessLogLine lts lelapsed lclientIP laction lsize lmethod lurl lident lhierarchy lmimeType
