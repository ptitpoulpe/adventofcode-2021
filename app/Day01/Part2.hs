{-# LANGUAGE ParallelListComp #-}

module Main where

import Data.List
import Text.Printf (printf)
import Util (inputRaw)

type Depth = Int

-- | Read the input file.
input :: String -> [Depth]
input = map read . lines . inputRaw

process :: [Depth] -> Int
process depths =
    let
        sums = [
            x + y + z
            | x <- depths
            | y <- drop 1 depths
            | z <- drop 2 depths
            ]
    in
        length $ filter (\z -> z) [
            x < y
            | x <- sums
            | y <- drop 1 sums
            ]

main :: IO ()
main = printf "Day01 Part2: %d" $ process $ input "inputs/01"
