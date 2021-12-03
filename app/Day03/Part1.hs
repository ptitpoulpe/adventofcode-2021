module Main where

import Data.List (transpose, foldl')
import Text.Printf (printf)
import Util (inputRaw)

-- | Read the input file.
input :: String -> [[Int]]
input = map parse . lines . inputRaw
    where
        bit '0' = 1
        bit '1' = -1
        parse = map bit

process :: [[Int]] -> Int
process lines = 
    (fromBinary $ map gamma sums) * (fromBinary $ map epsilon sums)
    where
        sums = map sum $ transpose lines
        gamma x | x > 0 = 1
        gamma x = 0
        epsilon x | x < 0 = 1
        epsilon x = 0
        fromBinary = foldl' (\acc b -> acc * 2 + b) 0

main :: IO ()
main = printf "Day03 Part1: %d" $ process $ input "inputs/03"