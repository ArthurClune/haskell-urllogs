{-# LANGUAGE OverloadedStrings #-}

module URLAlert.SquidLog
    (
      -- | This module parses Squid access.log files
      --
      
      -- * Functions for parsing lines
      squidLogLine,
      SquidLogLine(..),
      parseGZipLog,
    ) where

import Prelude hiding (takeWhile, take)
import Control.Applicative
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.Char8
import URLAlert.Utils as Utils
import URLAlert.Types

data Method = GET | HEAD| POST | PUT |
              CONNECT | ICP_QUERY | MNONE      
              deriving (Show, Eq)

-- | Store data about an access to a web resource
data SquidLogLine = SquidLogLine {
    -- ts        :: !Int,         
    -- | Store the requesting client's IP as a bytestring (for now)
    clientIP  :: {-# UNPACK #-} !S.ByteString,
    uri       :: URI   
} deriving (Show, Eq)

plainValue::Parser S.ByteString
plainValue = takeWhile1 (/= ' ')
{-# INLINE plainValue #-}

endValue::Parser S.ByteString
endValue = takeWhile1 (inClass "-a-z/")
{-# INLINE endValue #-}

-- parse a url.
urlValue1::Parser URI
urlValue1 = do 
        lscheme         <- "http://" .*> pure HTTP <|> "https://" .*> pure HTTPS
        lvhost          <- takeTill (== '/')
        (lPath, lParams) <- (,) <$> takeTill (== '?') <* char '?' <*> takeTill (== ' ')
                             <|> (,) <$> takeTill (== ' ') <*> pure ""
        return $! buildURI lvhost lPath lParams lscheme
{-# INLINE urlValue1 #-}

buildURI:: S.ByteString -> S.ByteString -> S.ByteString -> Scheme -> URI
buildURI lvhost lPath lParams lscheme  = 
    case a of
      [v]    -> case lscheme of 
                  HTTP  -> URI v lPath lParams 80 lscheme
                  HTTPS -> URI v lPath lParams 443 lscheme
      [v, p] -> URI v lPath lParams (toInt p) lscheme
      _      -> error "parse error"
    where
      a = S.split ':' lvhost

-- CONNECT type lines
urlValue2::Parser URI
urlValue2 = do
    (lvhost, lport) <- (,) <$> takeTill (==':') <* char ':' <*> takeTill (== ' ')
    return $! URI lvhost "/" "" (toInt lport) HTTPS
{-# INLINE urlValue2 #-}

-- | Parser for a single line from a squid logfile
squidLogLine::Parser SquidLogLine
squidLogLine = do
    lts        <- plainValue
    lelapsed   <- skipSpace *> plainValue
    lclientIP  <- space *> plainValue
    laction    <- space *> plainValue
    lsize      <- space *> plainValue
    lmethod    <- space *> (
                             string "GET"       *> pure GET  <|>
                             string "POST"      *> pure POST <|>
                             string "PUT"       *> pure PUT  <|>
                             string "HEAD"      *> pure HEAD <|>
                             string "CONNECT"   *> pure CONNECT   <|>
                             string "ICP_QUERY" *> pure ICP_QUERY <|>
                             string "NONE"      *> pure MNONE
                            )
    luri <- space *> urlValue1 <|> space *> urlValue2
    lident     <- space *> plainValue
    lhierarchy <- space *> plainValue
    lmimeType  <- space *> endValue
    return $! SquidLogLine lclientIP luri

instance LogFileParser SquidLogLine where
  parseLine = squidLogLine
