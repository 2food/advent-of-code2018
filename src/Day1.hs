module Day1 where

import           Util                           ( split )
import qualified Data.Text                     as T

-- Part 1

readInt :: String -> Int
readInt ""         = error "wæææ"
readInt ('+' : ns) = read ns :: Int
readInt ns         = read ns :: Int

readInts :: String -> Char -> [Int]
readInts "" _  = []
readInts s  sp = map (readInt . T.unpack . T.strip . T.pack) (split sp s)

frequency :: String -> Int
frequency s = sum $ readInts s ','

filename = "resources/day1-1.txt"

readIntFile :: String -> IO [Int]
readIntFile fn = do
    str <- readFile fn
    return (readInts str '\n')

frequencyFile :: IO Int
frequencyFile = do
    ints <- readIntFile filename
    return $ sum ints

