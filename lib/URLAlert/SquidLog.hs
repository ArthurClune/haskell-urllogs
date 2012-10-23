{-# LANGUAGE OverloadedStrings #-}

module URLAlert.SquidLog
    (
      -- | This module parses Squid access.log files
      --
      -- See http://www.linofee.org/~jel/proxy/Squid/accesslog.shtml for
      -- details of the log format

      -- * Functions for parsing lines
      squidLogLine,
      parseLines,
      parseLog,
      parseGZipLog,
    ) where

--import Debug.Trace (trace)

import Prelude hiding (takeWhile, take)

import Codec.Compression.GZip as GZip
import Control.Applicative
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL

import URLAlert.Types
import URLAlert.Utils (toInt, toStrict)

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
-- takes a bytestring, parses one line and returns the rest
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

parseLine::Parser SquidLogLine
parseLine = squidLogLine

parseLines :: SL.ByteString -> [Maybe SquidLogLine]
parseLines c = map (maybeResult . myParse . toStrict) (SL.lines c)
    where
      myParse s = feed (parse parseLine s) S.empty

-- | Read a gzip'd log file
parseGZipLog :: FilePath -> IO [Maybe SquidLogLine]
parseGZipLog f = do
    s <- fmap GZip.decompress (SL.readFile f)
    return (parseLines s)

-- | Read a plain log file
parseLog::FilePath -> IO [Maybe SquidLogLine]
parseLog f = do
    s <- SL.readFile f
    return (parseLines s)