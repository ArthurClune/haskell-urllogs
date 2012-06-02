--
-- read in ipoque url logs     
-- Sample data line
-- 
-- Jun  4 23:17:00 144.32.142.3 "CampusEast2 - 144.32.142.3"|host|144.32.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|"www.nap.edu"|"/images/footer_podicon.png"
-- 
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Text as T
import Data.Attoparsec.Text

import AJCUtils

data IpoqueLogLine = IpoqueLogLine {
    date  :: !T.Text,
    src   :: !T.Text,
    sport :: !Number,
    dst   :: !T.Text,
    dport :: !Number,
    vhost :: !T.Text,
    url   :: !T.Text
} deriving (Ord, Show, Eq)
    
quote, bar, colon :: Parser Char
quote  = satisfy (== '\"')
bar    = satisfy (== '|')
colon  = satisfy (== ':')
{-# INLINE quote #-}
{-# INLINE bar #-} 
{-# INLINE colon #-}

(~~)::T.Text -> T.Text -> T.Text
(~~) a b = T.append a b

quotedValue::Parser T.Text
quotedValue = (quote *> takeWhile1 (/= '\"')) <* quote
{-# INLINE quotedValue #-}

barValue::Parser T.Text
barValue = bar *> takeWhile1 (/= '|')
{-# INLINE barValue #-}    

hostPair::Parser (T.Text, Number)
hostPair = (,) <$> ((takeWhile1 (/= ':')) <* colon) <*> number
{-# INLINE hostPair #-}

dateValue::Parser (T.Text)
dateValue = concatDate <$> barValue <*> barValue <*> barValue <*> barValue <*> barValue <*> barValue
    where concatDate yr mth day hr mn sec = yr ~~ mth ~~ day ~~ hr ~~ mn ~~ sec
{-# INLINE dateValue #-}

ipoqueLineParser::Parser IpoqueLogLine
ipoqueLineParser = do
    skipWhile (/= '|')
    (lsrc, lsport) <- (barValue *> bar *> hostPair)
    (ldst, ldport) <- bar *> hostPair 
    ldate          <- dateValue   
    lvhost         <- bar *> quotedValue 
    lurl           <- bar *> quotedValue
    return $ IpoqueLogLine ldate lsrc lsport ldst ldport lvhost lurl
    --return $ IpoqueLogLine "1" lsrc lsport ldst ldport "6" "7"

