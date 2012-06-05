-- General Utils functions

module AJCUtils 
(
toStrict,
(~~)
)
where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL

(~~)::S.ByteString -> S.ByteString -> S.ByteString
(~~) = S.append
{-# INLINE (~~) #-}

toStrict::SL.ByteString->S.ByteString
toStrict = S.concat . SL.toChunks
{-# INLINE toStrict #-}
