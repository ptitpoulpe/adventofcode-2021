module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char(digitToInt)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.List.Split (splitOn)
import Data.List (group, sort, transpose, intercalate)


-- | Read the input file.
input :: String -> (Int, Int)
input fileName = (x, y)
    where
        parseLine line = read i
            where
                [_, i] = splitOn ": " line
        [x, y] = map parseLine $ lines $ inputRaw fileName


-- | Process
process :: (Int, Int) -> Int
process (x, y) = maximum [cx, cy]
    where
        scores = (Map.singleton (x, 0) 1, Map.singleton (y, 0) 1)
        (cx, cy) = process' scores

counts :: Map.Map Int Int
counts = foldr (\p m ->  Map.insertWith (+) p 1 m) Map.empty [x+y+z | x <- [1..3], y <- [1..3], z <- [1..3]]
win_value = 21

process' :: (Map.Map (Int, Int) Int, Map.Map (Int, Int) Int) -> (Int, Int)
process' (sx, sy)
    | Map.empty == sx && Map.empty == sy = (0, 0)
process' (sx, sy) = (cx + sum (Map.elems wsx) * sum (Map.elems sy), cy + sum (Map.elems wsy) * sum (Map.elems nsx))
    where
        new st = foldr (\(k, v) m -> Map.insertWith (+) k v m) Map.empty [(compute p s sd, c*sc) | ((p, s), c) <- Map.toList st, (sd, sc) <- Map.toList counts]
        (nsx, wsx) = Map.partitionWithKey (\(p, s) c -> s < win_value) $ new sx
        (nsy, wsy) = Map.partitionWithKey (\(p, s) c -> s < win_value) $ new sy
        (cx, cy) = process' (nsx, nsy)

compute :: Int -> Int -> Int -> (Int, Int)
compute p s d = (np, s+np)
    where
        np = (p + d - 1) `mod` 10 + 1

data State =
    State { p1::Int
          , p2::Int
          , s1::Int
          , s2::Int } deriving (Show, Eq, Ord)

main :: IO ()
main = printf "Day21 Part2: %s" $ show $ process $ input $ "inputs/21"
