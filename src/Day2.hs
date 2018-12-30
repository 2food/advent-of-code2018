module Day2 where

import qualified Data.Set                      as Set
import qualified Data.Map                      as Map


-- Part 1 

hashcount' :: String -> Map.Map Char Int -> Map.Map Char Int
hashcount' []       m = m
hashcount' (c : cs) m = hashcount' cs (Map.alter incval c m)
  where
    incval Nothing  = Just 1
    incval (Just n) = Just (n + 1)

hashcount :: String -> (String, Map.Map Char Int)
hashcount s = (s, hashcount' s Map.empty)

makesets :: [String] -> (Set.Set String, Set.Set String)
makesets ss =
    let hashcounts = map hashcount ss
    in  ( Set.fromList (map fst (filter hastwo hashcounts))
        , Set.fromList (map fst (filter hasthree hashcounts))
        )
  where
    hastwo (_, m) = 2 `elem` Map.elems m
    hasthree (_, m) = 3 `elem` Map.elems m

checksum :: [String] -> Int
checksum ss =
    let (twoset, threeset) = makesets ss in Set.size twoset * Set.size threeset


readIdFile :: String -> IO [String]
readIdFile filename = do
    str <- readFile filename
    return (lines str)

idfile = "resources/day2.txt"

getChecksum :: IO Int
getChecksum = do
    ids <- readIdFile idfile
    return (checksum ids)
