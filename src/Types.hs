{-# LANGUAGE BangPatterns #-}

module Types (
  URLAccess (..),
) where

import qualified Data.ByteString.Char8 as S

data URLAccess = URLAccess {
    -- ts        :: !Int,         
    clientIP  :: !S.ByteString,
    url       :: !S.ByteString
} deriving (Show, Eq)
