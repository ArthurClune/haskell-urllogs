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
    vhost     :: {-# UNPACK #-} !S.ByteString,
    uriPath   :: {-# UNPACK #-} !S.ByteString,
    uriParams :: {-# UNPACK #-} !S.ByteString,
    port      :: {-# UNPACK #-} !Int,
    scheme    :: Scheme
} deriving (Show, Eq)

-- | Store data about an access to a web resource
data URLAccess = URLAccess {
    -- ts        :: !Int,         
    clientIP  :: {-# UNPACK #-} !S.ByteString,
    uri       :: URI   
} deriving (Show, Eq)
