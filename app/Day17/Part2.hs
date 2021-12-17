module Main where

import qualified Data.Map as Map
import Data.Char(digitToInt)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)
import Data.List (group, sort)

data Square =
    Square { x1 :: Int
           , x2 :: Int
           , y1 :: Int 
           , y2 :: Int } deriving (Show)

-- | Read the input file.
input :: String -> Square
input fileName = Square (read x1) (read x2) (read y1) (read y2)
    where
        line = inputRaw fileName
        [_, coords] = splitOn "x=" line
        [cxs, cys] = splitOn ", y=" coords
        [x1, x2] = splitOn ".." cxs
        [y1, y2] = splitOn ".." cys

process :: Square -> Int
process (Square x1 x2 y1 y2) = length res
    where
        xss = [getXs 300 0 0 vx | vx <- [0..x2]]
        yss = [getYs 300 0 0 vy | vy <- [y1..200]]
        fxss = filter (any (\x -> x1 <= x && x <= x2)) xss
        fyss = filter (any (\y -> y1 <= y && y <= y2)) yss
        res = filter (any (\(x,y) -> x1 <= x && x <= x2 &&y1 <= y && y <= y2 )) [zip xs ys | xs <- fxss, ys <- fyss]

getXs ms s x vx
    | ms == s = [x]
    | otherwise = x:getXs ms (s+1) (x+vx) (newVx vx)
    where
        newVx vx 
            | vx == 0 = vx
            | vx > 0 = vx - 1
            | otherwise = vx + 1

getYs ms s y vy 
    | ms == s = [y]
    | otherwise = y:getYs ms (s+1) (y+vy) (newVy vy)
    where
        newVy vy = vy -1  

main :: IO ()
main = printf "Day16 Part1: %s" $ show $ process $ input "inputs/17" 
