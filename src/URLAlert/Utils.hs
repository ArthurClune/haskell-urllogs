-- General Utils functions

module URLAlert.Utils 
(
toStrict,
toInt,
getGZipLog,
getLog,
(~~)
)
where

import Data.Maybe
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as SL
import Codec.Compression.GZip as GZip

(~~)::S.ByteString -> S.ByteString -> S.ByteString
(~~) = S.append
{-# INLINE (~~) #-}

toStrict::SL.ByteString->S.ByteString
toStrict = S.concat . SL.toChunks
{-# INLINE toStrict #-}

toInt::S.ByteString->Int
toInt = fst . fromJust . S.readInt
{-# INLINE toInt #-}


-- | Read a gzip'd log file and apply parser p
getGZipLog::(SL.ByteString -> [b]) -> FilePath -> IO [b]
getGZipLog p f = do
    s <- fmap GZip.decompress (SL.readFile f)
    return (p s)

-- | Read a plain log file and apply parser p
getLog::(SL.ByteString -> [b]) -> FilePath -> IO [b]
getLog p f = do
    s <- SL.readFile f
    return (p s)

--version of toInt for debugging
--toInt::S.ByteString->Int
--toInt s = 
--    case n of
--        Nothing      -> 123456789987654321
--        Just (n2, _) -> n2 
--    where
--        n = S.readInt s
--{-# INLINE toInt #-}
