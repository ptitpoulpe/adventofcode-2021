module Main where
import qualified Data.Set as Set

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

fullGrid :: Grid -> Grid
fullGrid grid = Map.map (\x -> (x - 1) `mod` 9 + 1) fgrid 
    where 
        width = 1 + maximum (map fst $ Map.keys grid)
        height = 1 + maximum (map snd $ Map.keys grid)
        base = Map.assocs grid
        fgrid = Map.unions $ map (\(sx,sy) -> Map.fromList $ map (\((x,y), z) -> ((x+sx*width, y+sy*height), z + sx + sy)) base) [(sx, sy) | sx <- [0..4], sy <- [0..4] ]

process :: Grid -> Int
process grid = costs Map.! (width, height)
    where
        width = maximum $ map fst $ Map.keys grid
        height = maximum $ map snd $ Map.keys grid
        compute_costs costs = foldl (\acc x -> processCoord x grid acc) costs [(x,y) | x <- [0..width], y <- [0..height]]
        costs = foldl (\acc x -> compute_costs acc) Map.empty [0..10] -- dirty fix to handle shortest path not only right-down

processCoord :: (Int, Int) -> Grid -> Grid -> Grid
processCoord (0,0) grid costs = Map.insert (0,0) 0 costs
processCoord (x,y) grid costs = Map.insert (x,y) cost costs
    where
        pcosts = mapMaybe ((`Map.lookup` costs) . bimap (x +) (y +)) [(-1,0), (0,-1), (1, 0), (0, 1)]
        cost = (grid Map.! (x,y)) + minimum pcosts

main :: IO ()
main = printf "Day15 Part2: %s" $ show $ process $ fullGrid $ input "inputs/15"