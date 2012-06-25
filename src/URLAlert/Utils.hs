-- General Utils functions

module URLAlert.Utils 
(
toStrict,
toInt,
(~~)
)
where

import Data.Maybe
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL

(~~)::S.ByteString -> S.ByteString -> S.ByteString
(~~) = S.append
{-# INLINE (~~) #-}

toStrict::SL.ByteString->S.ByteString
toStrict = S.concat . SL.toChunks
{-# INLINE toStrict #-}

toInt::S.ByteString->Int
toInt = fst . fromJust . S.readInt
{-# INLINE toInt #-}

--version of toInt for debugging
--toInt::S.ByteString->Int
--toInt s = 
--    case n of
--        Nothing      -> 123456789987654321
--        Just (n2, _) -> n2 
--    where
--        n = S.readInt s
--{-# INLINE toInt #-}
