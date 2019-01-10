module Main where

import           Lib
import System.Environment (getArgs)
import Day3 (findOverlaps)

main :: IO ()
main = do
    (arg:args) <- getArgs
    case arg of 
        "day3" -> do 
            overlaps <- findOverlaps
            putStr ("Overlapping inches: " ++ show overlaps)
    return ()
