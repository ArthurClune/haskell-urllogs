--
-- read in ipoque url logs     
-- Sample data line
-- 
-- Jun  4 23:17:00 144.32.142.3 "CampusEast2 - 144.32.142.3"|host|144.32.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|"www.nap.edu"|"/images/footer_podicon.png"
-- 
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module URLAlert.IpoqueLog
    (
      ipoqueLogLine,
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
hostPair = (,) <$> ((takeWhile1 (/= ':')) <* colon) <*> takeWhile1 (/= '|')
{-# INLINE hostPair #-}

dateValue::Parser (S.ByteString)
dateValue = concatDate <$> barValue <*> barValue <*> barValue <*> barValue <*> barValue <*> barValue
    where concatDate yr mth day hr mn sec = yr ~~ mth ~~ day ~~ hr ~~ mn ~~ sec
{-# INLINE dateValue #-}

urlValue::Parser (S.ByteString, S.ByteString)
urlValue = (,) <$> takeTill (\c -> (c == '?') || (c =='\"')) <*> ( satisfy (== '?') *> takeTill (== '\"')  <|> quote *> pure "" )
{-# INLINE urlValue #-}

ipoqueLogLine::Parser URLAccess
ipoqueLogLine = do
    skipWhile (/= '|')
    (lsrc, lsport)  <- (barValue *> bar *> hostPair)
    (ldst, ldport)  <- bar *> hostPair 
    ldate           <- dateValue   
    lvhost          <- bar *> quotedValue 
    (lpath, lparams)<- bar *> quote *> urlValue
    return $! URLAccess lsrc (URI lvhost lpath lparams (toInt ldport) HTTP)

parseString::SL.ByteString -> [Maybe URLAccess]
parseString c = map (maybeResult . myParse . toStrict) (SL.lines c)
    where
      myParse s = feed (parse ipoqueLogLine s) S.empty


getGzipLog::FilePath -> IO [Maybe URLAccess]
getGzipLog f = do
    contents <- fmap GZip.decompress (SL.readFile f)
    let s = parseString contents
    return s

getLog::FilePath -> IO [Maybe URLAccess]
getLog f = do
    contents <- SL.readFile f
    let s = parseString contents
    return s      