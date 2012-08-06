
{-# LANGUAGE OverloadedStrings #-}

module URLAlert.IpoqueLog
    (
      -- | This library parses Ipoque PRX logs that have been sent to syslog
      -- 

      -- * Types
      IpoqueLogLine(..),
      -- * Functions for parsing lines

      ipoqueLogLine,
      parseLog,
      parseGZipLog
    ) where

import Prelude hiding (takeWhile, take)
import Control.Applicative
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.ByteString.Char8

import URLAlert.Utils as Utils
import URLAlert.Types

-- | Stores a Ipoque URL log line
data IpoqueLogLine = IpoqueLogLine {
    -- | Dates in textual format YYYYMMDDHHMMSS
    date     :: {-# UNPACK #-} !S.ByteString,
    -- | IP of client requesting this resource
    clientIP :: {-# UNPACK #-} !S.ByteString,
    -- | Port at the client end
    sport    :: {-# UNPACK #-} !Int,
    -- | IP of destination
    dst      :: {-# UNPACK #-} !S.ByteString,
    -- | The URI of the resource request
    uri      :: URI
} deriving (Show, Eq)

instance Ord IpoqueLogLine where
  l1 `compare` l2 = date l1 `compare` date l2

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

-- | Parser for a single log line
ipoqueLogLine::Parser IpoqueLogLine
ipoqueLogLine = do
    skipWhile (/= '|')
    (lsrc, lsport)  <- barValue *> bar *> hostPair
    (ldst, ldport)  <- bar *> hostPair 
    ldate           <- dateValue   
    lvhost          <- bar *> quotedValue 
    (lpath, lparams)<- bar *> quote *> urlValue
    return $ IpoqueLogLine ldate lsrc (toInt lsport) ldst
                (URI lvhost lpath lparams (toInt ldport) HTTP)


instance LogFileParser IpoqueLogLine where
  parseLine = ipoqueLogLine

