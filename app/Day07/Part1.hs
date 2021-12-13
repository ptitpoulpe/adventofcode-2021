
module Main where

import Data.List.Split (splitOn)
import Text.Printf (printf)
import Util (inputRaw)


-- | Read the input file.
input :: String -> [Int]
input fileName = map read $ (splitOn ",") $ inputRaw fileName


process :: [Int] -> Int
process positions = subProcess avg_pos avg_score positions
    where
        avg_pos = (sum positions) `div` (length positions)
        avg_score = score avg_pos positions


subProcess cur_pos cur_score positions
    | sup_score < cur_score = subProcess sup_pos sup_score positions
    | sub_score < cur_score = subProcess sub_pos sub_score positions
    | otherwise             = cur_score
    where
        sup_pos = cur_pos + 1
        sub_pos = cur_pos - 1
        sup_score = score sup_pos positions
        sub_score = score sub_pos positions


score :: Int -> [Int] -> Int
score position positions = sum [abs (position - p) | p <- positions]

main :: IO ()
main = printf "Day07 Part1: %s" $ show $ process $ input "inputs/07"