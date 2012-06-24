{-# LANGUAGE OverloadedStrings #-}
module URLAlert.AccessLog
    (
      -- | This module parses Squid access.log files
      --
      
      -- * Functions for parsing lines
      parseLines,
      -- * Functions for reading files
      URLAlert.AccessLog.getGZipLog,
      URLAlert.AccessLog.getLog
    ) where

import Prelude hiding (takeWhile, take)
import Control.Applicative
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL
import Data.Attoparsec.Char8
import URLAlert.Utils as Utils
import URLAlert.Types

data Method = GET | HEAD| POST | PUT |
              CONNECT | ICP_QUERY | MNONE      
              deriving (Show, Eq)

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

-- | Attoparsec parser for a single line from a squid logfile
accessLogLine::Parser URLAccess
accessLogLine = do
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
    return $! URLAccess lclientIP luri

-- | Parse a string containing a newline seperated set of lines from a squid log file
parseLines::SL.ByteString -> [Maybe URLAccess]
parseLines c = map (maybeResult . myParse . toStrict) (SL.lines c)
    where
      myParse s = feed (parse accessLogLine s) S.empty

-- | Read a gzip'd squid log file
getGZipLog::FilePath -> IO [Maybe URLAccess]
getGZipLog = Utils.getGZipLog parseLines

-- | Read a plain squid log file
getLog::FilePath -> IO [Maybe URLAccess]
getLog = Utils.getLog parseLines
