-- General Utils functions

module URLAlert.Utils 
(
toStrict,
toInt,
(~~)
)
where

--import Data.Maybe
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL

(~~)::S.ByteString -> S.ByteString -> S.ByteString
(~~) = S.append
{-# INLINE (~~) #-}

toStrict::SL.ByteString->S.ByteString
toStrict = S.concat . SL.toChunks
{-# INLINE toStrict #-}

-- | Convert a ByteString to an Int, return 0 if the parse fails
toInt::S.ByteString->Int
toInt s = case S.readInt s of
    Just (x, _) -> x
    Nothing     -> 0

{-# INLINE toInt #-}
