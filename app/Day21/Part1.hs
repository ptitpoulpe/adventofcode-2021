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
process values = last runs * nbruns
    where
        dice = cycle [1..10] -- 10 dice is same as 100
        meta_dice = cycle $ getShots 10 dice -- loop of 10 values
        runs = takeWhile (<1000) $ generate values (0, 0) meta_dice
        nbruns = (length runs + 1) * 3

generate (px,py) (sx,sy) (dx:dy:d) = nsx:nsy:(generate (npx, npy) (nsx, nsy) d)
    where
        npx = (px + dx - 1) `mod` 10 + 1
        npy = (py + dy - 1) `mod` 10 + 1
        nsx = sx+npx
        nsy = sy+npy
        

getShots :: Int -> [Int] -> [Int]
getShots 0 _ = []
getShots i xs = shot:(getShots (i-1) nxs)
    where
        (shot, nxs) = getShot xs

getShot :: [Int] -> (Int, [Int])
getShot (x:y:z:t) = (x + y + z, t)


main :: IO ()
main = printf "Day21 Part1: %s" $ show $ process $ input $ "inputs/21"
