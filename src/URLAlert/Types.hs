-- | The core types for the URL application

module URLAlert.Types (
  URLAccess (..),
  URI(..),
  Scheme(..)
) where

import qualified Data.ByteString.Char8 as S

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

-- | Store data about an access to a web resource
data URLAccess = URLAccess {
    -- ts        :: !Int,         
    -- | Store the requesting client's IP as a bytestring (for now)∆ca
    clientIP  :: {-# UNPACK #-} !S.ByteString,
    uri       :: URI   
} deriving (Show, Eq)
