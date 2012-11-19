-- | The core types for the URL application

module URLAlert.Types (
  -- | This module provides types for handling URL log records

  -- * Core Types
  URL(..),
  Scheme(..),
  Method(..),
  SquidLogLine(..),

) where

import qualified Data.ByteString.Char8 as S

-- | The scheme used to access the resource
data Scheme = HTTP | HTTPS | NONE deriving (Show, Eq) 

-- | Stores a URL
data URL = URL {
    -- | The vhost requested (e.g. www.bbc.co.uk)
    vhost     :: {-# UNPACK #-} !S.ByteString,
    -- | The path part of the request (e.g. /iplayer)
    uriPath   :: {-# UNPACK #-} !S.ByteString,
    -- | The parameters of the request (e.g. ?a=b)
    uriParams :: {-# UNPACK #-} !S.ByteString,
    -- | The port connected to on the server (e.g. 80)
    port      :: {-# UNPACK #-} !Int,
    -- | Either HTTP, HTTPS or NONE for error lines
    scheme    :: Scheme
} deriving (Show, Eq)

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
    uri       :: URL,
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
