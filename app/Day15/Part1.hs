module Main where

import qualified Data.Map as Map
import Data.Char(digitToInt)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)

type Grid = Map.Map (Int, Int) Int

-- | Read the input file.
input :: String -> Grid
input fileName = Map.fromList [((x, y), grid!!x!!y) | x <- [0..length grid - 1],
                                                     y <- [0..length (head grid) - 1]]
    where
        grid = map (map digitToInt) $ lines $ inputRaw fileName

process :: Grid -> Int
process grid = costs  Map.! (width, height)
    where
        width = maximum $ map fst $ Map.keys grid
        height = maximum $ map snd $ Map.keys grid
        costs = foldl (\acc x -> processCoord x grid acc) Map.empty [(x,y) | x <- [0..width], y <- [0..height]]

processCoord :: (Int, Int) -> Grid -> Grid -> Grid
processCoord (x,y) grid costs = Map.insert (x,y) cost costs
    where
        pcosts = mapMaybe ((`Map.lookup` costs) . bimap (x +) (y +)) [(-1,0), (0,-1)]
        cost = if null pcosts then 0 else (grid Map.! (x,y)) + minimum pcosts

main :: IO ()
main = printf "Day15 Part1: %s" $ show $ process $ input "inputs/15"
