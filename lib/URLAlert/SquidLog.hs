{-# LANGUAGE OverloadedStrings #-}

module URLAlert.SquidLog
    (
      -- | This module parses Squid access.log files
      --
      -- See http://www.linofee.org/~jel/proxy/Squid/accesslog.shtml for
      -- details of the log format

      -- * Functions for parsing lines
      runParse,
      squidLogLine
    ) where

--import Debug.Trace (trace)

import Prelude hiding (takeWhile, take)

import Control.Applicative
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as S

import URLAlert.Types

ipv6host::Parser S.ByteString
ipv6host = "[" .*> takeWhile1 (inClass "a-f0-9:%") <*. "]"
{-# INLINE ipv6host #-}

-- parse a vhost.
-- sample vhost values
--[fe80::215:99ff:fe4a:3d45%2513]:80
--[fe80::215:99ff:fe4a:3d45%2513]
-- www.bbc.co.uk:80
-- www.bbc.co.uk
parseVHost::Parser (S.ByteString, Int)
parseVHost = do
  (lvhost, lport) <-    (,) <$> ipv6host <*>  (":" .*> decimal)
                    <|> (,) <$> ipv6host <*>  pure 0
                    <|> (,) <$> (takeWhile1 (/= ':') <*. ":") 
                              <*> decimal
                    <|> (,) <$> takeWhile1 (\x -> x /= '/' && x /= ' ') 
                              <*> pure 0
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
            0 -> case lscheme of
                HTTP  -> return $ URI lvhost lpath lparams 80 lscheme
                HTTPS -> return $ URI lvhost lpath lparams 443 lscheme
                _     -> error "Parse failed in urlValue1"
            _ -> return $ URI lvhost lpath lparams lport lscheme
{-# INLINE urlValue1 #-}

-- CONNECT type lines
urlValue2::Parser URI
urlValue2 = do
    lvhost <- takeTill (== ':') <* char ':' 
    lport  <- decimal
    return $ URI lvhost "/" "" lport HTTPS
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
    lts        <- decimal <* takeWhile1 (/= ' ')
    lelapsed   <- skipSpace *> decimal
    lclientIP  <- space *> takeWhile1 (/= ' ')
    laction    <- space *> takeWhile1 (/= '/') <*. "/"
    lresult    <- decimal
    lsize      <- space *> decimal
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
    lident     <- space *> takeWhile1 (/= ' ')
    lhierarchy <- space *> takeWhile1 (/= '/') <*. "/"
    lremip     <- takeWhile1 (/= ' ')
    lmimeType  <- space *> takeWhile (/= ' ')
    return $ SquidLogLine lts lelapsed lclientIP 
                    laction lresult lsize lmethod 
                    luri lident lhierarchy lremip lmimeType

-- | Parse a single line from a bytestring
runParse :: S.ByteString -> Maybe SquidLogLine
runParse s = maybeResult $ feed (parse squidLogLine s) S.empty
