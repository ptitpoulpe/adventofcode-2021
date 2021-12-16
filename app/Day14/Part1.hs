module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char (isUpper)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)


type Rules = Map.Map (Char, Char) Char

-- | Read the input file.
input :: String -> (String, Rules)
input fileName = (polymer, Map.fromList $ map parseRules $ lines rules)
    where
        [polymer, rules] = splitOn "\n\n" $ inputRaw fileName

parseRules :: String -> ((Char, Char), Char)
parseRules s = ((s1, s2), d)
    where
        [[s1, s2], [d]] = splitOn " -> " s

process :: String -> Rules -> Int
process polymer rules = max - min
    where
        result = foldl (\p _ -> processStep p rules) polymer [1..10]
        sizes = Map.fromList $ Set.toList $ Set.map (\x -> (length $ filter (==x) result, x)) $ Set.fromList result
        (max, _) = Map.findMax sizes
        (min, _) = Map.findMin sizes

processStep :: String -> Rules -> String 
processStep polymer rules = filter (/='#') $ concat $ map (\(x,y) -> [x,y]) $ zip inserts polymer
    where
        pairs = zip (' ':polymer) $ polymer
        inserts = map (\seq -> Map.findWithDefault '#' seq rules) pairs

main :: IO ()
main = printf "Day14 Part1: %s" $ show $ process polymer rules
    where
        (polymer, rules) = input "inputs/14"