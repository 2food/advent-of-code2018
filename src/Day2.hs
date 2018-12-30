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

-- Part 2

equalWithTolerance :: Int -> String -> String -> Bool
equalWithTolerance _ [] [] = True
equalWithTolerance _ [] _  = False
equalWithTolerance _ _ []  = False
equalWithTolerance n (x:xs) (y:ys) | x == y    = equalWithTolerance n xs ys
                                   | otherwise = n > 0 && equalWithTolerance (n-1) xs ys

isOneOffFrom :: String -> String -> Bool
isOneOffFrom = equalWithTolerance 1

allOneOff' :: [String] -> [String] -> [String]
allOneOff' [] acc = acc
allOneOff' (s:ss) acc | any matches acc || any matches ss = allOneOff' ss (s:acc)
                      | otherwise                         = allOneOff' ss acc
                        where matches = isOneOffFrom s

allOneOff :: [String] -> [String]
allOneOff ss = allOneOff' ss []

common :: String -> String -> String
common s1 s2 = map fst (filter (uncurry (==)) (zip s1 s2))

getCommon :: IO String
getCommon = do
    ids <- readIdFile idfile
    let [s1, s2] = allOneOff ids -- in the given data there are only two similar ids
    return (common s1 s2)