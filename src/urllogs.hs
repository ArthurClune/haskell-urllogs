
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as S
import qualified Data.Conduit.List as DCL
import Data.Conduit
import qualified Data.HashTable.IO as H
import Data.List (sortBy)
import System.Environment (getArgs)
import Safe (abort)
import Text.Printf (printf)

import qualified URLAlert.SquidLog as SquidLog
import URLAlert.Types

type HashTable k v = H.BasicHashTable k v

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
    ht   <- H.newSized(1000000)::IO(HashTable S.ByteString Int)
    let counter = count ht
    logLines <- SquidLog.parseGZipLog file
    DCL.sourceList logLines $= DCL.catMaybes 
                                 $= DCL.filter (\x -> mimeType x == "text/html")
                                 $$ DCL.mapM_ counter
    lst <- H.toList ht                            
    mapM_ putStrLn . zipWith pretty [1..] $ topNList 100 lst
  where 
    count ht l = do
      val <- H.lookup ht key
      case val of
        Nothing -> H.insert ht key 1
        Just v  -> H.insert ht key (v+1)
      where
        key = vhost . uri $ l





