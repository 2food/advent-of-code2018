module Util (split) where

split :: Char -> String -> [String]
split _ "" = []
split c s  = takeWhile (/= c) s : split c (drop 1 (dropWhile (/= c) s))