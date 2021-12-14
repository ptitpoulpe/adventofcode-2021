
module Main where

import qualified Data.Map as Map
import Data.Char(digitToInt)
import Text.Printf (printf)
import Util (inputRaw)
import GHC.IO.FD (release)

type Grid = Map.Map (Int, Int) Int

-- | Read the input file.
input :: String -> Grid
input fileName = Map.fromList [((x, y), grid!!x!!y) | x <- [0..length grid - 1],
                                                     y <- [0..length (head grid) - 1]]
    where
        grid = map (map digitToInt) $ lines $ inputRaw fileName


process grid = fc
    where
        (fc, fg) = foldr (\x y -> step y) (0, grid) [1..100]
    
step (c, grid) = (c + cf, fgrid)
    where
        igrid = increment grid
        f = flashes igrid
        rgrid = releaseEnergy f igrid
        cf = countFlash rgrid
        fgrid = clean rgrid


increment :: Grid -> Grid
increment = Map.map (+1)

flashes :: Grid -> [(Int, Int)]
flashes grid = Map.keys $ Map.filter (==10) grid

releaseEnergy :: [(Int, Int)] -> Grid -> Grid
releaseEnergy ((x,y):flashes) grid = releaseEnergy flashes $ foldr adjust grid $ map (\(sx, sy) -> (x+sx, y+sy)) directions
    where
        directions = [(-1,-1), (-1, 0), (-1, 1),
                      ( 0,-1),          ( 0, 1),
                      ( 1,-1), ( 1, 0), ( 1, 1)]
        adjust (nx,ny) agrid 
            | Map.findWithDefault 0 (nx, ny) adjusted == 10 = releaseEnergy [(nx, ny)] adjusted
            | otherwise = adjusted
            where
                adjusted = Map.adjust (+1) (nx,ny) agrid
releaseEnergy [] grid = grid

countFlash :: Grid -> Int 
countFlash grid = Map.size $ Map.filter (>9) grid

clean :: Grid -> Grid
clean grid = Map.map (\x -> if x>9 then 0 else x) grid

main :: IO ()
main = printf "Day11 Part1: %s" $ show $ process $ input "inputs/11"