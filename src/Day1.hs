module Day1 where

import           Util                           ( split
                                                , dup
                                                )
import qualified Data.Text                     as T
import           Data.Maybe                     ( fromMaybe )

-- Part 1

readInt :: String -> Int
readInt ""         = error "wæææ"
readInt ('+' : ns) = read ns :: Int
readInt ns         = read ns :: Int

readInts :: String -> Char -> [Int]
readInts "" _  = []
readInts s  sp = map (readInt . T.unpack . T.strip . T.pack) (split sp s)

filename = "resources/day1.txt"

readIntFile :: String -> IO [Int]
readIntFile fn = do
    str <- readFile fn
    return (readInts str '\n')

frequency :: IO Int
frequency = do
    ints <- readIntFile filename
    return $ sum ints

-- Part 2

getSums :: [Int] -> [Int]
getSums ns = 0 : accsum 0 ns
  where
    accsum i []       = []
    accsum i (n : ns) = i + n : accsum (i + n) ns

findDup :: [Int] -> Int
findDup ns = fromMaybe (error "file doesn't have a duplicate!") (dup ns)

repeatFreqUntilDup :: [Int] -> Int
repeatFreqUntilDup ns = fromMaybe (repeatFreqUntilDup (ns ++ ns)) (dup (getSums ns))

firstDuplicateFrequency :: IO Int
firstDuplicateFrequency = do
    ints <- readIntFile filename
    return $ repeatFreqUntilDup ints
