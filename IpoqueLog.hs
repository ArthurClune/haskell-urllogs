--
-- read in ipoque url logs     
-- Sample data line
-- 
-- Jun  4 23:17:00 144.32.142.3 "CampusEast2 - 144.32.142.3"|host|144.32.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|"www.nap.edu"|"/images/footer_podicon.png"
-- 
{-# LANGUAGE BangPatterns #-}
 --{-# LANGUAGE OverloadedStrings #-}

module IpoqueLog
    (
      IpoqueLogLine,
      date,
      src,
      dst,
      sport,
      dport,
      vhost, 
      url,
      ipoqueLineParser,
      toStrict
    ) where


import Control.Applicative
import qualified Data.Attoparsec.Lazy as A
import Data.Attoparsec.Char8 hiding (space, take)
import qualified Data.ByteString.Char8 as S

import AJCUtils

data IpoqueLogLine = IpoqueLogLine {
    date  :: !S.ByteString,
    src   :: !S.ByteString,
    sport :: !S.ByteString,
    dst   :: !S.ByteString,
    dport :: !S.ByteString,
    vhost :: !S.ByteString,
    url   :: !S.ByteString        
} deriving (Ord, Show, Eq)
    
quote, bar, space, colon :: Parser Char
quote  = satisfy (== '\"')
bar    = satisfy (== '|')
space  = satisfy (== ' ')
colon  = satisfy (== ':')
{-# INLINE quote #-}
{-# INLINE bar #-} 
{-# INLINE space #-}
{-# INLINE colon #-}

plainValue::Parser S.ByteString
plainValue = takeWhile1 (/= ' ')
{-# INLINE plainValue #-}

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

ipoqueLineParser::Parser IpoqueLogLine
ipoqueLineParser = do
    skipWhile (/= '|')
    (src, sport) <- (barValue *> bar *> hostPair)
    (dst, dport) <- bar *> hostPair 
    date <- dateValue   
    vhost <- bar *> quotedValue 
    url   <- bar *> quotedValue
    return $ IpoqueLogLine date src sport dst dport vhost url
