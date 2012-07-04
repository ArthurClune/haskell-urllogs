-- | The core types for the URL application

module URLAlert.Types (
  -- | This module provides types for handling URL log records

  -- * Core Types
  URI(..),
  Scheme(..),
  -- * Typeclasses
  LogFileParser(..),
) where


import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL
import Data.Attoparsec.Char8
import URLAlert.Utils (toStrict)
import Codec.Compression.GZip as GZip

-- | The scheme used to access the resource
data Scheme = HTTP | HTTPS deriving (Show, Eq) 

-- | Stores a URI 
data URI = URI {
    -- | The vhost requested (e.g. www.bbc.co.uk)
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

-- | Typeclass for URL log parsers
--
-- Users must define a parser parseLine that parses a single line from the logfile
class LogFileParser b where
  -- | Parse a Bytestring contraining a single line
  parseLine::Parser b

  -- | Parse a string containing a newline seperated set of lines
  parseLines::SL.ByteString -> [Maybe b]
  parseLines c = map (maybeResult . myParse . toStrict) (SL.lines c)
      where
        myParse s = feed (parse parseLine s) S.empty

  -- | Read a gzip'd log file
  parseGZipLog::FilePath -> IO [Maybe b]
  parseGZipLog f = do
      putStrLn "hi"
      s <- fmap GZip.decompress (SL.readFile f)
      return (parseLines s)

  -- | Read a plain log file
  parseLog::FilePath -> IO [Maybe b]
  parseLog f = do
      s <- SL.readFile f
      return (parseLines s)

