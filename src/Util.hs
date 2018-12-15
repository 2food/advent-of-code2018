module Util
    ( split
    , dup
    )
where

import qualified Data.Set                      as S

split :: Char -> String -> [String]
split _ "" = []
split c s  = takeWhile (/= c) s : split c (drop 1 (dropWhile (/= c) s))

dup' :: (Ord a) => [a] -> S.Set a -> Maybe a
dup' []       _   = Nothing
dup' (x : xs) set = if S.member x set then Just x else dup' xs (S.insert x set)

dup :: (Ord a) => [a] -> Maybe a
dup xs = dup' xs S.empty