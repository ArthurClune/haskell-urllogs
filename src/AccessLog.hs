-- parse standard squid logfiles 
-- see http://www.linofee.org/~jel/proxy/Squid/accesslog.shtml for log format

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module AccessLog
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
import AJCUtils (toStrict)
import Types

plainValue::Parser S.ByteString
plainValue = takeWhile1 (/= ' ')
{-# INLINE plainValue #-}

endValue::Parser S.ByteString
endValue = takeWhile1 (inClass "a-z/ ")
{-# INLINE endValue #-}

preludeString::Parser Char
preludeString = plainValue *> skipSpace *> plainValue *> space

accessLogLine::Parser URLAccess
accessLogLine = do
    lclientIP  <- preludeString *> plainValue
    lurl       <- space *> plainValue *> space *> plainValue *> space *> plainValue *> space *> plainValue <* endValue
    return $ URLAccess lclientIP lurl

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

