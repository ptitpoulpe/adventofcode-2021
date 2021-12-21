module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char(digitToInt)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.List.Split (splitOn)
import Data.List (group, sort, transpose, partition)
import Data.Either (partitionEithers)
import qualified GHC.Stable as Set

data Point =
    Point { x :: Int
          , y :: Int
          , z :: Int } deriving (Eq, Ord, Show)

data Scanner =
    Scanner { name :: String
            , points :: Set.Set Point
            , vectors :: Map.Map Point [(Int, Point, Point)] } deriving (Show)

-- | Read the input file.
input :: String -> [Scanner]
input fileName = map parseScanner scanners
    where
        scanners = splitOn "\n\n" $ inputRaw fileName

parseScanner :: String -> Scanner
parseScanner s = Scanner name (Set.fromList points) vectors
    where
        (name:points_) = lines s
        points = map parsePoint points_
        vectors = getVectors points

getVectors points = Map.fromListWith (++) [ (rot, [(r, src, dst)])
                    | src@(Point sx sy sz) <- points,
                      dst@(Point dx dy dz) <- points,
                      src /= dst,
                      (r, rot) <- orientations (Point (dx-sx) (dy-sy) (dz-sz))]

parsePoint :: String -> Point
parsePoint s  = Point x y z
    where
        [x, y, z] = map read $ splitOn "," s

-- | Process
process (scanner:scanners) = Set.size $ process' [scanner] scanners


process' :: [Scanner] -> [Scanner] -> Set.Set Point
process' s@((Scanner sname spoints svectors):todo) scanners = Set.union spoints (process' (todo++ntodo) nooverlaps)
    where
        svector = Map.filter (/=[]) $ Map.map (filter (\(r, _, _) -> r==1)) svectors
        testScanner o@(Scanner oname opoints ovectors) = res
            where
                interFunc os ss = concat [[(oi, (ssrc, osrc)), (oi, (sdst, odst))] | (oi, osrc, odst) <- os, (si, ssrc, sdst) <- ss]
                inter = Map.intersectionWith interFunc ovectors svector
                beacons_by_rot = Map.fromListWith Set.union $ map (\(i, p) -> (i, Set.singleton p) ) $ concat $ Map.elems inter
                valid_rots = Map.filter (\x -> 12 <= length x ) beacons_by_rot
                validTrans ((r, so):_) = Left (o, r, t)
                    where
                        u = Set.map (\(p1, p2) -> (p1, orientate r p2)) so
                        t = head $ Set.elems $ Set.map (\(Point sx sy sz, Point ox oy oz) -> Point (sx-ox) (sy-oy) (sz-oz)) u
                validTrans [] = Right o
                res = validTrans $ Map.toList valid_rots
        (overlaps, nooverlaps) = partitionEithers $ map testScanner scanners
        ntodo = map (\(o, r, t) -> transform r t o) overlaps
process' [] _ = Set.empty

transform rot (Point sx sy sz) (Scanner name points vectors) = Scanner name new_points new_vectors
    where
        trans p = Point (x+sx) (y+sy) (z+sz)
            where
                (Point x y z) = orientate rot p
        new_points = Set.map trans points
        new_vectors = getVectors $ Set.toList new_points

translate :: Point -> [Point] -> [Point]
translate point values = map (transPoint point) values
    where
        transPoint (Point sx sy sz) (Point x y z) = Point (sx+x) (sy+y) (sz+z)

orientations :: Point -> [(Int, Point)]
orientations p = map (\i -> (i, orientate i p)) [1..24]

orientate :: Int -> Point -> Point
orientate  i (Point x y z)
    | i ==  1 = Point   x    y    z
    | i ==  2 = Point   x  (-y) (-z)
    | i ==  3 = Point   x    z  (-y)
    | i ==  4 = Point   x  (-z)   y
    | i ==  5 = Point (-x)   y  (-z)
    | i ==  6 = Point (-x) (-y)   z
    | i ==  7 = Point (-x)   z    y
    | i ==  8 = Point (-x) (-z) (-y)
    | i ==  9 = Point   y    x  (-z)
    | i == 10 = Point   y  (-x)   z
    | i == 11 = Point   y    z    x
    | i == 12 = Point   y  (-z) (-x)
    | i == 13 = Point (-y)   x    z
    | i == 14 = Point (-y) (-x) (-z)
    | i == 15 = Point (-y)   z  (-x)
    | i == 16 = Point (-y) (-z)   x
    | i == 17 = Point   z    x    y
    | i == 18 = Point   z  (-x) (-y)
    | i == 19 = Point   z    y  (-x)
    | i == 20 = Point   z  (-y)   x
    | i == 21 = Point (-z)   x  (-y)
    | i == 22 = Point (-z) (-x)   y
    | i == 23 = Point (-z)   y    x
    | i == 24 = Point (-z) (-y) (-x)

main :: IO ()
main = printf "Day19 Part1: %s" $ show $ process $ input "inputs/19"
