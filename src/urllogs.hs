
{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import qualified Data.ByteString.Char8 as S
--import qualified Data.Conduit.List as DCL
--import Data.Conduit
import qualified Data.HashMap.Strict as M
import Data.List (foldl', sortBy)
import System.Environment (getArgs)
import Safe (abort)
import Text.Printf (printf)

--import Debug.Trace (traceShow)

import qualified URLAlert.SquidLog as SquidLog
import URLAlert.Types
--import qualified URLAlert.IpoqueLog as IpoqueLog

-- quick and dirty command line args handling
parseArgs::IO String
parseArgs = do
    args <- getArgs
    if length args == 1
        then return (args !! 0)
        else abort "Usage: urllogs.hs [file1]"

-- | TopN 
-- Return a list of the lines matching cond, with key given by the field "field" 
matchLines :: (a -> S.ByteString) -> (a -> Bool) -> [Maybe a] -> [(S.ByteString, Int)]
matchLines field cond = M.toList . foldl' count M.empty
    where
        count !acc l = case l of
            Just x -> if cond x
                      then M.insertWith (+) (S.copy (field x)) 1 acc
                      else acc
            Nothing -> acc

-- Helper that sorts a list based on the second value 
-- and returns the top N values
topNList :: Ord b => Int -> [(a, b)] -> [(a, b)]
topNList n l = take n $ sortBy mostPopular l
  where
    mostPopular (_,a) (_,b) = compare b a

---- Helper for printing the top list.
pretty :: Show a => Int -> (a, Int) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n

--step :: Int -> SquidLog.SquidLogLine -> Int
--step acc l = (SquidLog.size l) + acc

main::IO()
main = do      
    file <- parseArgs
    logLines <- SquidLog.parseGZipLog file::IO [Maybe SquidLog.SquidLogLine]
    let vhosts = matchLines (vhost . SquidLog.uri) (\x -> SquidLog.mimeType x == "text/html")
                    logLines
    mapM_ putStrLn . zipWith pretty [1..] $ topNList 100 vhosts
    --print $ foldl (+) 0 $ map bytes logLines
    --y <- DCL.sourceList logLines $= DCL.catMaybes $$ DCL.fold step 0
    --print y
