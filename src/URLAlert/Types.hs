{-# LANGUAGE BangPatterns #-}

module URLAlert.Types (
  URLAccess (..),
  URI(..),
  Scheme(..)
) where

import qualified Data.ByteString.Char8 as S

data Scheme = HTTP | HTTPS deriving (Show, Eq)

data URI = URI {
    vhost     :: {-# UNPACK #-} !S.ByteString,
    uriPath   :: {-# UNPACK #-} !S.ByteString,
    uriParams :: {-# UNPACK #-} !S.ByteString,
    port      :: {-# UNPACK #-} !Int,
    scheme    :: Scheme
} deriving (Show, Eq)

data URLAccess = URLAccess {
    -- ts        :: !Int,         
    clientIP  :: {-# UNPACK #-} !S.ByteString,
    uri       :: URI   
} deriving (Show, Eq)
