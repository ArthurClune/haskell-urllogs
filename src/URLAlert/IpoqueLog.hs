
{-# LANGUAGE OverloadedStrings #-}

module URLAlert.IpoqueLog
    (
      -- | This library parses Ipoque PRX logs that have been sent to syslog

      -- * Functions for parsing lines
      ipoqueLogLine,
      IpoqueLogLine(..),
      parseGZipLog,
    ) where

import Prelude hiding (takeWhile, take)
import Control.Applicative
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.ByteString.Char8

import URLAlert.Utils as Utils
import URLAlert.Types

data IpoqueLogLine = IpoqueLogLine {
    -- ts        :: !Int,         
    -- | Store the requesting client's IP as a bytestring (for now)
    clientIP  :: {-# UNPACK #-} !S.ByteString,
    uri       :: URI   
} deriving (Show, Eq)


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

-- Parser for a single log line
ipoqueLogLine::Parser IpoqueLogLine
ipoqueLogLine = do
    skipWhile (/= '|')
    (lsrc, lsport)  <- barValue *> bar *> hostPair
    (ldst, ldport)  <- bar *> hostPair 
    ldate           <- dateValue   
    lvhost          <- bar *> quotedValue 
    (lpath, lparams)<- bar *> quote *> urlValue
    return $! IpoqueLogLine lsrc (URI lvhost lpath lparams (toInt ldport) HTTP)


instance LogFileParser IpoqueLogLine where
  parseLine = ipoqueLogLine

