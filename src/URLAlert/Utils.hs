-- General Utils functions

module URLAlert.Utils 
(
toStrict,
toInt,
parseLogFile,
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

-- read a log file and apply a transform function t to the bytestring from the file
-- e.g. ungzip before appling the parser p
parseLogFile :: (SL.ByteString -> SL.ByteString) -> (SL.ByteString -> [b]) -> FilePath -> IO [b]
parseLogFile t p file = do
    contents <- fmap t (SL.readFile file)
    let s = p contents
    return s

--version of toInt for debugging
--toInt::S.ByteString->Int
--toInt s = 
--    case n of
--        Nothing      -> 123456789987654321
--        Just (n2, _) -> n2 
--    where
--        n = S.readInt s
--{-# INLINE toInt #-}
