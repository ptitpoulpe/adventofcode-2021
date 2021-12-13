
module Main where

import qualified Data.Set as Set
import Data.Char(digitToInt)
import Data.List (sort)
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Util (inputRaw)

-- | Read the input file.
input :: String -> [[Int]]
input fileName = map (map digitToInt) $ lines $ inputRaw fileName


process grid = x * y * z
    where
        width = length grid
        heigh = length $ head grid
        bottoms = filter (\(x, y) -> bottom grid x y) [(x,y) | x <- [0..width - 1], y <- [0..heigh - 1]]
        x:y:z:_ = reverse $ sort $ map (\(x, y) -> Set.size $ bassin grid x y Set.empty) bottoms

bottom grid x y = left && right && up && down
    where
        current = grid!!x!!y
        left = if x == 0 then True else current < grid!!(x-1)!!y
        right = if x == ((length grid) - 1) then True else current < grid!!(x+1)!!y
        up = if y == 0 then True else current < grid!!x!!(y-1)
        down = if y == ((length $ head grid) - 1) then True else current < grid!!x!!(y+1)

bassin :: [[Int]] -> Int -> Int -> Set.Set (Int, Int) -> Set.Set (Int, Int)
bassin grid x y done = done5
    where
        current = grid!!x!!y
        done1 = Set.union done (Set.fromList [(x, y)])
        lefts = if x == 0 || current > grid!!(x-1)!!y || grid!!(x-1)!!y == 9 || ((x-1, y) `Set.member` done1) then Set.empty else (bassin grid (x-1) y done1)
        done2 = Set.union done1 lefts
        rights = if x == ((length grid) - 1) || current > grid!!(x+1)!!y || grid!!(x+1)!!y == 9 || ((x+1, y) `Set.member` done2)  then Set.empty else bassin grid (x+1) y done2
        done3 = Set.union done2 rights
        ups = if y == 0 || current > grid!!x!!(y-1) || grid!!x!!(y-1) == 9 || ((x, y-1) `Set.member` done3) then Set.empty else bassin grid x (y-1) done3
        done4 = Set.union done3 ups
        downs = if y == ((length $ head grid) - 1) || current > grid!!x!!(y+1) || grid!!x!!(y+1) == 9 || ((x, y+1) `Set.member` done4) then Set.empty else bassin grid x (y+1) done4
        done5 = Set.union done4 downs

main :: IO ()
main = printf "Day09 Part9: %s" $ show $ process $ input "inputs/09"