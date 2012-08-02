
{-# LANGUAGE OverloadedStrings #-}

--import Control.Monad
import qualified Data.ByteString.Char8 as S
import qualified Data.HashMap.Strict as M
import Data.List (foldl', sortBy)
import System.Environment
import Safe
import Text.Printf (printf)

import qualified URLAlert.SquidLog as SquidLog
import URLAlert.Types
--import qualified URLAlert.IpoqueLog as IpoqueLog

-- | Apply function f to a list of list of lines ls 
-- combining the results with g
analyseFiles :: (a -> b) -> ([b] -> c) -> [a] -> c
analyseFiles f g ls = g $ map f ls

parseArgs::IO [FilePath]
parseArgs = do
    args <- getArgs
    if length args > 0
        then return args
        else abort "Usage: urllogs.hs [file1] <file2> ...."

-- | TopN 
-- Return a list of the lines matching cond, with key given by the field "field" 
topN :: (a->S.ByteString) -> (a->Bool) -> [Maybe a] -> [(S.ByteString,Int)]
topN field cond = M.toList . foldl' count M.empty
    where
        count acc l = case l of
            Just x -> if cond x
                      then M.insertWith (+) (S.copy (field x)) 1 acc
                      else acc
            Nothing -> acc

-- Helper that turns a map into a top list, based on the second value 
-- and returns the top N values
topNList :: Ord b => Int -> [(a, b)] -> [(a, b)]
topNList n l = do
   take n $ sortBy mostPopular l
  where
    mostPopular (_,a) (_,b) = compare b a

---- Helper for printing the top list.
pretty :: Show a => Int -> (a, Int) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n

main::IO()
main = do      
    files <- parseArgs
    logLines <- mapM SquidLog.parseGZipLog files::IO [[Maybe SquidLog.SquidLogLine]]
    --let len1 = analyseFiles length (foldl (+) 0) logLines
    let vhosts = analyseFiles (topN (vhost . SquidLog.uri) (\x -> if SquidLog.mimeType x == "text/html" then True else False)) id logLines
    -- todo map this across multiple files. 
    -- just take top N*2 from each file, then top N from result
    mapM_ putStrLn . zipWith pretty [1..] $ topNList 100 (vhosts !! 0)

