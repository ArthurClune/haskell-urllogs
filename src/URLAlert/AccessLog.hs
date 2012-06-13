-- parse standard squid logfiles 
-- see http://www.linofee.org/~jel/proxy/Squid/accesslog.shtml for log format

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module URLAlert.AccessLog
    (
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
import URLAlert.Utils (toInt, toStrict)
import URLAlert.Types

data Method = GET | HEAD| POST | PUT |
              CONNECT | ICP_QUERY | MNONE      
              deriving (Show, Eq)

plainValue::Parser S.ByteString
plainValue = takeWhile1 (/= ' ')
{-# INLINE plainValue #-}

endValue::Parser S.ByteString
endValue = takeWhile1 (inClass "a-z/")
{-# INLINE endValue #-}

accessLogLine::Parser URLAccess
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
    return $ URLAccess lclientIP (S.pack "hi") (S.pack "hi") Nothing 3 HTTP


parseFile::SL.ByteString -> [Maybe URLAccess]
parseFile c = map (maybeResult . parse accessLogLine . toStrict) (SL.lines c)

getGzipLog::FilePath -> IO [Maybe URLAccess]
getGzipLog f = do
    contents <- fmap GZip.decompress (SL.readFile f)
    let s = parseFile contents
    return s

getLog::FilePath -> IO [Maybe URLAccess]
getLog f = do
    contents <- SL.readFile f
    let s = parseFile contents
    return s
