{-# LANGUAGE OverloadedStrings #-}

module URLAlert.SquidLog
    (
      -- | This module parses Squid access.log files
      --
      -- See http://www.linofee.org/~jel/proxy/Squid/accesslog.shtml for
      -- details of the log format

      -- * Types
      Method,
      SquidLogLine(..),
      -- * Functions for parsing lines
      squidLogLine,
      parseGZipLog,
    ) where

import Prelude hiding (takeWhile, take)
import Control.Applicative
import qualified Data.ByteString.Char8 as S
import Data.Attoparsec.Char8
import URLAlert.Utils (toInt)
import URLAlert.Types

-- | Store the type of HTTP request made
data Method = GET | HEAD| POST | PUT |
              CONNECT | ICP_QUERY | MNONE      
              deriving (Show, Eq)

-- | Store data about an access to a web resource from a squid log file
data SquidLogLine = SquidLogLine {
    -- | Time of request to nearest second
    ts        :: {-# UNPACK #-} !Int,     
    -- | Elapsed time to fulfil request      
    elapsed   :: {-# UNPACK #-} !Int,
    -- | IP of client requesting this resource
    clientIP  :: {-# UNPACK #-} !S.ByteString,
    -- | Action (e.g. cache miss)
    action    :: {-# UNPACK #-} !S.ByteString,
    -- | Size of result (bytes)
    size      :: {-# UNPACK #-} !Int,
    -- | Method of request
    method    :: Method,
    -- | URI requested
    uri       :: URI,
    -- | The result of the RFC931/ident lookup of the client username. 
    -- If RFC931/ident lookup is disabled (default: `ident_lookup off'), it is logged as - .
    ident     :: {-# UNPACK #-} !S.ByteString,
    -- | A description of how and where the requested object was fetched.
    -- This includes the IP of the remote server and should be parsed further
    hierarchy :: {-# UNPACK #-} !S.ByteString,
    -- | Mimetype of result
    mimeType  :: {-# UNPACK #-} !S.ByteString
} deriving (Show, Eq)

instance Ord SquidLogLine where
    l1 `compare` l2 = (ts l1) `compare` (ts l2)

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
    return $! SquidLogLine (toInt lts) (toInt lelapsed) lclientIP 
                    laction (toInt lsize) lmethod luri lident
                    lhierarchy lmimeType

instance LogFileParser SquidLogLine where
  parseLine = squidLogLine
