module Day3 where

import           Util                           ( split )
import qualified Data.Map.Strict               as Map

type ID = Int
type Width = Int
type Height = Int
type MarginLeft = Int
type MarginTop = Int

data Claim = C ID MarginLeft MarginTop Width Height deriving Show

type Pos = (Int, Int)
type PatchMap = Map.Map Pos Int 
data Patch = P [ID] PatchMap deriving Show
emptyPatch = P [] Map.empty

claimfile = "resources/day3.txt"


readID ('#' : id) = (read id) :: ID
readMargs s =
    let (mleft : mtop : _) = split ',' s
    in  (read mleft :: Int, read (init mtop) :: Int)
readArea a =
    let (width : height : _) = split 'x' a
    in  (read width :: Width, read height :: Height)

readClaim :: String -> Claim
readClaim s = case words s of
    (i : _ : margs : area : _) ->
        let (mleft, mtop) = readMargs margs
        in  let (width, height) = readArea area
            in  C (readID i) mleft mtop width height
    _ -> error $ "couln't read claim " ++ s

readClaims :: IO [Claim]
readClaims = do
    file <- readFile claimfile
    let claims = map readClaim (lines file)
    return claims

claimToPoss :: Claim -> [Pos]
claimToPoss (C _ ml mt w h) = [(x,y) | x <- [ml+1..ml+w],
                                       y <- [mt+1..mt+h]]

addPoss :: PatchMap -> [Pos] -> PatchMap
addPoss = foldr (\p pm -> Map.insertWith (+) p 1 pm)

addClaim ::  Claim -> Patch -> Patch
addClaim (C id ml mt w h) (P ids pmap) = 
    P (id : ids) (addPoss pmap (claimToPoss (C id ml mt w h)))

addClaims :: [Claim] -> Patch -> Patch
addClaims cs p = foldr addClaim p cs

countOverlappingInches :: Patch -> Int
countOverlappingInches (P _ pmap) = 
    foldr (\x acc -> if x > 1 then (acc+1) else acc) 0 pmap

findOverlaps :: IO Int
findOverlaps = do
    claims <- readClaims
    let santacloth = addClaims claims emptyPatch
    return (countOverlappingInches santacloth)