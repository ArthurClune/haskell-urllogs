-- | The core types for the URL application

module URLAlert.Types (
  -- | This module provides types for handling URL log records

  -- * Core Types
  URLAccess (..),
  URI(..),
  Scheme(..),
  -- * Typeclasses
  LogFileParser,
  parseLines,
  parseGZipLog,
  parseLog
) where


import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL
import Data.Attoparsec.Char8
import URLAlert.Utils
import Codec.Compression.GZip as GZip

-- | The scheme used to access the resource
data Scheme = HTTP | HTTPS deriving (Show, Eq) 

-- | Stores a URI 
data URI = URI {
    --  The vhost requested (e.g. www.bbc.co.uk)
    vhost     :: {-# UNPACK #-} !S.ByteString,
    -- | The path part of the request (e.g. /iplayer)
    uriPath   :: {-# UNPACK #-} !S.ByteString,
    -- | The parameters of the request (e.g. ?a=b)
    uriParams :: {-# UNPACK #-} !S.ByteString,
    -- | The port connected to on the server (e.g. 80)
    port      :: {-# UNPACK #-} !Int,
    -- | Either HTTP or HTTPS for now
    scheme    :: Scheme
} deriving (Show, Eq)

-- | Store data about an access to a web resource
data URLAccess = URLAccess {
    -- ts        :: !Int,         
    -- | Store the requesting client's IP as a bytestring (for now)
    clientIP  :: {-# UNPACK #-} !S.ByteString,
    uri       :: URI   
} deriving (Show, Eq)

-- | Typeclass for URL log parsers
--
-- Users must define a parser that parses a single line from the logfile
class LogFileParser b where

  -- | Parse a string containing a newline seperated set of lines
  parseLines::Parser b -> SL.ByteString -> [Maybe b]
  parseLines p c = map (maybeResult . myParse . toStrict) (SL.lines c)
      where
        myParse s = feed (parse p s) S.empty

  -- | Read a gzip'd log file
  parseGZipLog::Parser b -> FilePath -> IO [Maybe b]
  parseGZipLog p f = do
      s <- fmap GZip.decompress (SL.readFile f)
      return (parseLines p s)

  -- | Read a plain log file
  parseLog::Parser b -> FilePath -> IO [Maybe b]
  parseLog p f = do
      s <- SL.readFile f
      return (parseLines p s)

instance LogFileParser URLAccess
