-- parse standard squid logfiles 
-- see http://www.linofee.org/~jel/proxy/Squid/accesslog.shtml for log format

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module AccessLog
    (
      AccessLogLine (..),
      Method (..),
      accessLogLine,
      getGzipLog,
      getLog
    ) where


import Prelude hiding (takeWhile, take)
import Control.Applicative
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL
import Data.Attoparsec.Char8
import qualified Codec.Compression.GZip as GZip
import AJCUtils (toInt, toStrict)

data Method = GET | HEAD| POST | PUT |
              CONNECT | ICP_QUERY | MNONE      
              deriving (Show, Eq)

data AccessLogLine = AccessLogLine {
    ts        :: !Int,           -- we round time to nearest second
    elapsed   :: !Int,
    clientIP  :: !S.ByteString,
    action    :: !S.ByteString,
    size      :: !Int,
    method    :: Method,
    url       :: !S.ByteString,
    ident     :: !S.ByteString,
    hierarchy :: !S.ByteString,
    mimeType  :: !S.ByteString
} deriving (Show, Eq)

plainValue::Parser S.ByteString
plainValue = takeWhile1 (/= ' ')
{-# INLINE plainValue #-}

endValue::Parser S.ByteString
endValue = takeWhile1 (inClass "a-z/")
{-# INLINE endValue #-}

accessLogLine::Parser AccessLogLine
accessLogLine = do
    lts        <- plainValue
    lelapsed   <- skipSpace *> plainValue
    lclientIP  <- space *> plainValue
    laction    <- space *> plainValue
    lsize      <- space *> plainValue
    lmethod    <- space *> (string "GET"       *> pure GET  <|>
                            string "POST"      *> pure POST <|>
                            string "PUT"       *> pure PUT  <|>
                            string "HEAD"      *> pure HEAD <|>
                            string "CONNECT"   *> pure CONNECT   <|>
                            string "ICP_QUERY" *> pure ICP_QUERY <|>
                            string "NONE"      *> pure MNONE
                            )
    lurl       <- space *> plainValue
    lident     <- space *> plainValue
    lhierarchy <- space *> plainValue
    lmimeType  <- space *> endValue
    return $ AccessLogLine (toInt lts) (toInt lelapsed) lclientIP laction 
                           (toInt lsize) lmethod lurl lident lhierarchy lmimeType

parseFile::SL.ByteString -> [Maybe AccessLogLine]
parseFile c = map (maybeResult . parse accessLogLine . toStrict) (SL.lines c)

getGzipLog::FilePath -> IO [Maybe AccessLogLine]
getGzipLog f = do
    contents <- fmap GZip.decompress (SL.readFile f)
    let s = parseFile contents
    return s

getLog::FilePath -> IO [Maybe AccessLogLine]
getLog f = do
    contents <- SL.readFile f
    let s = parseFile contents
    return s

