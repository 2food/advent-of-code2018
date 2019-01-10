module Day3 where

import           Data.Matrix                   as M
import           Util                           ( split )

type ID = Int
type Width = Int
type Height = Int
type Area = Matrix Int

data Claim = C ID Area deriving Show

data Patch = P [ID] Area

claimfile = "resources/day3.txt"


readID ('#' : id) = (read id) :: ID
readMargs s =
    let (mleft : mtop : _) = split ',' s
    in  (read mleft :: Int, read (init mtop) :: Int)
readArea s =
    let (width : height : _) = split 'x' s
    in  M.fromList (read width :: Width) (read height :: Height) (repeat 1)

readClaim :: String -> Claim
readClaim s = case words s of
    (i : _ : margs : area : _) ->
        let (mleft, mtop) = (readMargs margs)
         in let (width, height) = (readArea area)
        in  C (readID i) (makeMatrix mleft mtop width height) 
    _ -> error $ "couln't read claim " ++ s

readClaims :: IO [Claim]
readClaims = do
    file <- readFile claimfile
    let claims = map readClaim (lines file)
    return claims

addClaim :: Patch -> Claim -> Patch
addClaim (P ids parea) (C id carea) = P (id:ids) (addMatrices parea carea)



countOverlappedInches :: Matrix Int -> Int
countOverlappedInches m = undefined

findOverlaps :: IO Int
findOverlaps = do
    claims <- readClaims
    let m = genMatrix claims
    printMatrix m
    return (countOverlappedInches m)

someMatrix :: Matrix Int
someMatrix = M.matrix 5 5 (const 0)

someOtherMatrix :: Matrix Int
someOtherMatrix = M.matrix 10 10 (const 1)

printMatrix m = putStr $ M.prettyMatrix m ++ "\n"

adjustMatrixSizes :: Matrix Int -> Matrix Int -> (Matrix Int, Matrix Int)
adjustMatrixSizes m1 m2 | M.nrows m1 > M.nrows m2 = 
    adjustMatrixSizes (M.extendTo 0 (M.nrows m2) (M.nrows m1) m1) m2
                        | otherwise = (m1, m2)

addMatrices :: Matrix Int -> Matrix Int -> Matrix Int
addMatrices m1 m2 = let (m1', m2') = adjustMatrixSizes m1 m2 in 
     M.elementwiseUnsafe (+) m1' m2'