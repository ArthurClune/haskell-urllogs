{-# LANGUAGE BangPatterns #-}

module URLAlert.Types (
  URLAccess (..),
  Scheme(..)
) where

import qualified Data.ByteString.Char8 as S

data Scheme = HTTP | HTTPS deriving (Show, Eq)

data URLAccess = URLAccess {
    -- ts        :: !Int,         
    clientIP  :: !S.ByteString,
    vhost     :: !S.ByteString,
    uriPath   :: !S.ByteString,
    uriParams :: Maybe S.ByteString,
    port      :: !Int,
    scheme    :: Scheme
} deriving (Show, Eq)
