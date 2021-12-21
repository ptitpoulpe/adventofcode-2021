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

type ImageEnhancementAlgorithm = [Bool]

type Code = Int

type Coord = (Int, Int)

data Grid = 
    Grid { values :: Map.Map Coord Bool
         , infinite:: Bool }

-- | Read the input file.
input :: String -> (ImageEnhancementAlgorithm, Grid)
input fileName = (iea, Grid grid False)
    where
        [iea_, grid_] = splitOn "\n\n" $ inputRaw fileName
        iea = map ('#'==) iea_
        grid = Map.fromList [ ((x, y), '#'==c)
                            | (y, line) <- enumerate $ lines grid_
                            , (x, c) <- enumerate line]

enumerate :: [a] -> [(Int, a)]
enumerate x = enum 0 x
    where
        enum i (y:ys) = (i, y):(enum (i+1) ys)
        enum _ [] = []

showG :: Grid -> Grid
showG grid@(Grid values infinite) = pTraceShow ( "\n" ++ intercalate "\n" [intercalate "" [if Map.findWithDefault infinite (x,y) values then "#" else "." | x <- xs] | y <- ys]) grid
    where
        xs_ = map fst $ Map.keys values
        ys_ = map snd $ Map.keys values
        xs = [minimum xs_ - 1..maximum xs_ + 1]
        ys = [minimum ys_ - 1..maximum ys_ + 1]

-- | Process

process iea grid = Map.size $ Map.filter id $ values $ showG last_grid
    where
        last_grid = foldl (\acc x -> process' iea acc) grid [1..2]

process' :: ImageEnhancementAlgorithm -> Grid -> Grid
process' iea grid@(Grid values infinite) = Grid (Map.fromList res) new_infinite
    where
        xs_ = map fst $ Map.keys values
        ys_ = map snd $ Map.keys values
        xs = [minimum xs_ - 1..maximum xs_ + 1]
        ys = [minimum ys_ - 1..maximum ys_ + 1]
        new_infinite = iea !! (code (minimum xs_ - 2, 0) grid)
        res = [((x,y), iea !! (code (x, y)) grid) | y <- ys, x <- xs]


code :: Coord -> Grid -> Int
code (x, y) (Grid grid infinite) = foldl (\acc b -> acc * 2 + (if b then 1 else 0)) 0 binCode
    where 
        binCode = [Map.findWithDefault infinite (x+sx, y+sy) grid | sy <- [-1..1], sx <- [-1..1]]

main :: IO ()
main = printf "Day20 Part1: %s" $ show $ process iea grid
    where
        (iea, grid) = input "inputs/20"
