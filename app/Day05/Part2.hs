module Main where

import qualified Data.Map as Map  
import Text.Regex.TDFA
import Text.Printf (printf)
import Util (inputRaw)

data Direction = Forward | Up | Down deriving (Enum, Show)

data Vector =
    Vector { src :: Point
           , dst :: Point } deriving (Show)

data Point =
    Point { xi :: Int
          , yi :: Int } deriving (Show, Eq, Ord)

-- | Read the input file.
input :: String -> [Vector] --, String, String, String)]
input = map parse . lines . inputRaw

vectorRe = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)"
parse :: String -> Vector
parse line = Vector (Point (read srcX) (read srcY)) (Point (read dstX) (read dstY))
    where
        (_, _, _ , [srcX, srcY, dstX, dstY]) = line =~ vectorRe :: (String, String, String, [String])


-- | Process 
process :: [Vector] -> Int
process vectors =
    Map.size $ Map.filter (>=2) $ subProcess vectors

subProcess :: [Vector] -> Map.Map Point Int
subProcess (vector:vectors) =
    foldr (\p m -> Map.insertWith (+) p 1 m) sub_map (positions vector)
    where
        sub_map = subProcess vectors
subProcess [] = Map.empty

positions :: Vector -> [Point]
positions (Vector (Point srcX srcY) (Point dstX dstY))
    | srcX == dstX && srcY < dstY = map (\y -> Point srcX y) [srcY..dstY]
    | srcX == dstX                = map (\y -> Point srcX y) [dstY..srcY]
    | srcY == dstY && srcX < dstX = map (\x -> Point x srcY) [srcX..dstX]
    | srcY == dstY                = map (\x -> Point x srcY) [dstX..srcX]
    | srcX < dstX && srcY < dstY  = map (\(x, y) -> Point x y) $ zip [srcX..dstX] [srcY..dstY]
    | srcX < dstX                 = map (\(x, y) -> Point x y) $ zip [srcX..dstX] (reverse [dstY..srcY])
    | srcY < dstY                 = map (\(x, y) -> Point x y) $ zip (reverse [dstX..srcX]) [srcY..dstY]
    | otherwise                   = map (\(x, y) -> Point x y) $ zip (reverse [dstX..srcX]) (reverse [dstY..srcY])

main :: IO ()
main = printf "Day05 Part1: %s" $ show $ process $ input "inputs/05"
