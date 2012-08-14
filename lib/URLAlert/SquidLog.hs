{-# LANGUAGE OverloadedStrings #-}

module URLAlert.SquidLog
    (
      -- | This module parses Squid access.log files
      --
      -- See http://www.linofee.org/~jel/proxy/Squid/accesslog.shtml for
      -- details of the log format

      -- * Types
      Method(..),
      SquidLogLine(..),
      -- * Functions for parsing lines
      squidLogLine,
      parseLog,
      parseGZipLog,
    ) where

--import Debug.Trace (trace)

import Prelude hiding (takeWhile, take)

import Control.Applicative
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as S

import URLAlert.Types
import URLAlert.Utils (toInt)

-- | Store the type of HTTP request made
data Method = GET | HEAD| POST | PUT | OPTIONS |
              CONNECT | ICP_QUERY | MNONE | PROPFIND     
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
    -- | HTTP result code for this request
    resultCode:: {-# UNPACK #-} !Int,
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
    hierarchy :: {-# UNPACK #-} !S.ByteString,
    -- This includes the IP of the remote server 
    remIP :: {-# UNPACK #-} !S.ByteString,
    -- | Mimetype of result
    mimeType  :: {-# UNPACK #-} !S.ByteString
} deriving (Show, Eq)

instance Ord SquidLogLine where
    l1 `compare` l2 = ts l1 `compare` ts l2

instance LogFileParser SquidLogLine where
  parseLine = squidLogLine

plainValue::Parser S.ByteString
plainValue = takeWhile1 (/= ' ')
{-# INLINE plainValue #-}

slashPair::Parser (S.ByteString, S.ByteString)
slashPair = (,) <$> (space *> takeWhile1 (/= '/') <*. "/") <*> takeWhile1 (/= ' ')
{-# INLINE slashPair #-}

endValue::Parser S.ByteString
endValue = takeWhile (/= ' ')
{-# INLINE endValue #-}

-- parse a vhost.
-- sample vhost values
--[fe80::215:99ff:fe4a:3d45%2513]:80
--[fe80::215:99ff:fe4a:3d45%2513]
-- www.bbc.co.uk:80
-- www.bbc.co.uk
parseVHost::Parser (S.ByteString, S.ByteString)
parseVHost = do
  (lvhost, lport) <-    (,) <$> ipv6host <*>  (satisfy (== ':') *> takeWhile1 (/= '/'))
                    <|> (,) <$> ipv6host <*>  pure "__not__"
                    <|> (,) <$> (takeWhile1 (\x -> x /= ':' && x /= '/') <*. ":") 
                              <*> takeWhile1 (\x -> x /= '/' && x /= ' ')
                    <|> (,) <$> takeWhile1 (\x -> x /= '/' && x /= ' ') 
                              <*> pure "__not__"
  return (lvhost, lport)
{-# INLINE parseVHost #-}


-- parse a url.
urlValue1::Parser URI
urlValue1 = do 
        lscheme         <- "http://" .*> pure HTTP <|> "https://" .*> pure HTTPS
        (lvhost, lport) <- parseVHost
        (lpath, lparams) <- (,) <$> takeTill (== '?') <* char '?' <*> takeTill (== ' ')
                             <|> (,) <$> takeTill (== ' ') <*> pure ""
        case lport of
            "__not__" -> case lscheme of
                HTTP  -> return $ URI lvhost lpath lparams 80 lscheme
                HTTPS -> return $ URI lvhost lpath lparams 443 lscheme
                _     -> error "Parse failed in urlValue1"
            _ -> return $ URI lvhost lpath lparams (toInt lport) lscheme
{-# INLINE urlValue1 #-}

ipv6host::Parser S.ByteString
ipv6host = satisfy (== '[') *> takeWhile1 (/= ']') <* satisfy (== ']')
{-# INLINE ipv6host #-}

-- CONNECT type lines
urlValue2::Parser URI
urlValue2 = do
    (lvhost, lport) <- (,) <$> takeTill (== ':') <* char ':' <*> takeWhile1 isDigit
    return $ URI lvhost "/" "" (toInt lport) HTTPS
{-# INLINE urlValue2 #-}

-- NONE type lines
-- 20 Jun 2012 06:56:37 192.168.76.250 400 NONE error:request-too-large text/html
urlValue3::Parser URI
urlValue3 = do
  text <- takeTill (== ' ')
  return $ URI text "" "" 0 NONE

-- | Parser for a single line from a squid logfile
squidLogLine::Parser SquidLogLine
squidLogLine = do
    lts        <- plainValue
    lelapsed   <- skipSpace *> plainValue
    lclientIP  <- space *> plainValue
    (laction, lresult) <- slashPair
    lsize      <- space *> plainValue
    lmethod    <- space *> (
                             string "GET"       *> pure GET  <|>
                             string "POST"      *> pure POST <|>
                             string "PUT"       *> pure PUT  <|>
                             string "HEAD"      *> pure HEAD <|>
                             string "CONNECT"   *> pure CONNECT   <|>
                             string "ICP_QUERY" *> pure ICP_QUERY <|>
                             string "NONE"      *> pure MNONE <|>
                             string "OPTIONS"   *> pure OPTIONS <|>
                             string "PROPFIND"  *> pure PROPFIND 
                            )
    luri       <- space *> urlValue1 <|> space *> urlValue2 <|> space *> urlValue3
    lident     <- space *> plainValue
    (lhierarchy, lremip) <- slashPair
    lmimeType  <- space *> endValue
    return $ SquidLogLine (toInt lts) (toInt lelapsed) lclientIP 
                    laction (toInt lresult) (toInt lsize) lmethod 
                    luri lident lhierarchy lremip lmimeType
