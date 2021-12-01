module Main where

import Text.Printf (printf)
import Util (inputRaw)

type Depth = Int

-- | Read the input file.
input :: String -> [Depth]
input = map read . lines . inputRaw

process :: [Depth] -> Int
process depths =
    length $ filter id $ zipWith (<) depths (drop 1 depths)

main :: IO ()
main = printf "Day01 Part1: %d" $ process $ input "inputs/01"
