
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as S
import qualified Data.Conduit.List as DCL
import Data.Conduit
import qualified Data.HashMap.Strict as M
import Data.List (sortBy)
import System.Environment (getArgs)
import Safe (abort)
import Text.Printf (printf)

--import Debug.Trace (traceShow)

import qualified URLAlert.SquidLog as SquidLog
import URLAlert.Types

-- quick and dirty command line args handling
parseArgs::IO String
parseArgs = do
    args <- getArgs
    if length args == 1
        then return (head args)
        else abort "Usage: urllogs.hs [file1]"

-- Helper that sorts a list based on the second value 
-- and returns the top N values
topNList :: Ord b => Int -> [(a, b)] -> [(a, b)]
topNList n l = take n $ sortBy mostPopular l
  where
    mostPopular (_,a) (_,b) = compare b a

---- Helper for printing the top list.
pretty :: Show a => Int -> (a, Int) -> String
pretty i (bs, n) = printf "%d: %s, %d" i (show bs) n

main::IO()
main = do      
    file <- parseArgs
    logLines <- SquidLog.parseGZipLog file
    y <- DCL.sourceList logLines $= DCL.catMaybes 
                                 $= DCL.filter (\x -> mimeType x == "text/html")
                                 $$ DCL.fold count M.empty
    mapM_ putStrLn . zipWith pretty [1..] $ topNList 100 (M.toList y)
  where 
    count acc l = M.insertWith (+) (S.copy (vhost . uri $ l)) 1 acc

