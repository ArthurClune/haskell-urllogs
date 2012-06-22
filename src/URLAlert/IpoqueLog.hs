
{-# LANGUAGE OverloadedStrings #-}

-- | This library parses Ipoque PRX logs that have been sent to syslog
module URLAlert.IpoqueLog
    (
      parseLines,
      getGzipLog,
      getLog
    ) where

import Prelude hiding (takeWhile, take)
import Control.Applicative
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL
import Data.Attoparsec.ByteString.Char8
import qualified Codec.Compression.GZip as GZip

import URLAlert.Utils
import URLAlert.Types

quote, bar, colon :: Parser Char
quote  = satisfy (== '\"')
bar    = satisfy (== '|')
colon  = satisfy (== ':')
{-# INLINE quote #-}
{-# INLINE bar #-} 
{-# INLINE colon #-}

quotedValue::Parser S.ByteString
quotedValue = (quote *> takeWhile1 (/= '\"')) <* quote
{-# INLINE quotedValue #-}

barValue::Parser S.ByteString
barValue = bar *> takeWhile1 (/= '|')
{-# INLINE barValue #-}    

hostPair::Parser (S.ByteString, S.ByteString)
hostPair = (,) <$> (takeWhile1 (/= ':') <* colon) <*> takeWhile1 (/= '|')
{-# INLINE hostPair #-}

dateValue::Parser S.ByteString
dateValue = concatDate <$> barValue <*> barValue <*> barValue <*> barValue <*> barValue <*> barValue
    where concatDate yr mth day hr mn sec = yr ~~ mth ~~ day ~~ hr ~~ mn ~~ sec
{-# INLINE dateValue #-}

urlValue::Parser (S.ByteString, S.ByteString)
urlValue = (,) <$> takeTill (\c -> (c == '?') || (c =='\"')) <*> ( satisfy (== '?') *> takeTill (== '\"')  <|> quote *> pure "" )
{-# INLINE urlValue #-}

-- Attoparsec parser for a single log line
ipoqueLogLine::Parser URLAccess
ipoqueLogLine = do
    skipWhile (/= '|')
    (lsrc, lsport)  <- barValue *> bar *> hostPair
    (ldst, ldport)  <- bar *> hostPair 
    ldate           <- dateValue   
    lvhost          <- bar *> quotedValue 
    (lpath, lparams)<- bar *> quote *> urlValue
    return $! URLAccess lsrc (URI lvhost lpath lparams (toInt ldport) HTTP)

-- | Parse a string containing a newline seperated set of lines from an Ipoque PRX
-- to syslog into component parts
parseLines::SL.ByteString -> [Maybe URLAccess]
parseLines c = map (maybeResult . myParse . toStrict) (SL.lines c)
    where
      myParse s = feed (parse ipoqueLogLine s) S.empty

-- | Read a gzip'd log file
getGzipLog::FilePath -> IO [Maybe URLAccess]
getGzipLog = parseLogFile GZip.decompress parseLines

-- | Read a plain log file
getLog::FilePath -> IO [Maybe URLAccess]
getLog = parseLogFile id parseLines
