--
-- read in ipoque url logs     
-- Sample data line
-- 
-- Jun  4 23:17:00 144.32.142.3 "CampusEast2 - 144.32.142.3"|host|144.32.34.125:60326|144.171.20.6:80|2011|06|04|23|17|00|"www.nap.edu"|"/images/footer_podicon.png"
-- 

{-# LANGUAGE BangPatterns #-}
 --{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.Attoparsec as A
import Data.Attoparsec.Char8 hiding (space, take)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL

import qualified Codec.Compression.GZip as GZip


(~~)::B.ByteString -> B.ByteString -> B.ByteString
(~~) = B.append
{-# INLINE (~~) #-}

toStrict::BL.ByteString->B.ByteString
toStrict = B.concat . BL.toChunks
{-# INLINE toStrict #-}

data LogLine = LogLine {
    date  :: !B.ByteString,
    src   :: !B.ByteString,
    sport :: !B.ByteString,
    dst   :: !B.ByteString,
    dport :: !B.ByteString,
    vhost :: !B.ByteString,
    url   :: !B.ByteString        
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

plainValue::Parser B.ByteString
plainValue = takeWhile1 (/= ' ')
{-# INLINE plainValue #-}

quotedValue::Parser B.ByteString
quotedValue = (quote *> takeWhile1 (/= '\"')) <* quote
{-# INLINE quotedValue #-}

barValue::Parser B.ByteString
barValue = bar *> takeWhile1 (/= '|')
{-# INLINE barValue #-}    

hostPair::Parser (B.ByteString, B.ByteString)
hostPair = do
    host <- takeWhile1 (/= ':') <* colon
    port <- takeWhile1 (/= '|')  
    return (host, port)
{-# INLINE hostPair #-}

line::Parser LogLine
line = do    
    (src, sport) <- takeWhile1 (/= '|') *> (barValue *> hostPair)
    (dst, dport) <- bar *> hostPair    
    yr  <- barValue
    mth <- barValue
    day <- barValue
    hr  <- barValue
    mn  <- barValue
    sec <- barValue
    vhost <- bar *> quotedValue 
    url   <- bar *> quotedValue
    return $ LogLine (yr ~~ mth ~~ day ~~ hr ~~ mn ~~ sec) src sport dst dport vhost url

main = do         
    contents <- fmap GZip.decompress (BL.readFile "/home/arthur/Work/data/url.log.1.gz")
    mapM_ (print . maybeResult . A.parse line . toStrict) (SL.lines contents)
