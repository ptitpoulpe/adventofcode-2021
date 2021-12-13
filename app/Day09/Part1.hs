
module Main where

import Data.Char(digitToInt)
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Util (inputRaw)


-- | Read the input file.
input :: String -> [[Int]]
input fileName = map (map digitToInt) $ lines $ inputRaw fileName


process grid = sum $ map (\(x, y) -> bottom grid x y) [(x,y) | x <- [0..width - 1], y <- [0..heigh - 1]]
    where
        width = length grid
        heigh = length $ head grid

bottom grid x y = if left && right && up && down then (current + 1) else 0
    where
        current = grid!!x!!y
        left = if x == 0 then True else current < grid!!(x-1)!!y
        right = if x == ((length grid) - 1) then True else current < grid!!(x+1)!!y
        up = if y == 0 then True else current < grid!!x!!(y-1)
        down = if y == ((length $ head grid) - 1) then True else current < grid!!x!!(y+1)


main :: IO ()
main = printf "Day08 Part1: %s" $ show $ process $ input "inputs/09"