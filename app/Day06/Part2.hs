module Main where

import Data.List.Split (splitOn)
import Text.Printf (printf)
import Util (inputRaw)


-- | Read the input file.
input :: String -> [Int]
input fileName = map (\x -> length $ filter (==x) timers) [0..8]
    where
        timers = map read $ (splitOn ",") $ inputRaw fileName


process :: Int -> [Int] -> Int
process 0 timers = sum timers
process days [x0, x1, x2, x3, x4, x5, x6, x7, x8] =
    process (days - 1) [x1, x2, x3, x4, x5, x6, x7 + x0, x8, x0]

main :: IO ()
main = printf "Day06 Part2: %s" $ show $ process 256 $ input "inputs/06"
