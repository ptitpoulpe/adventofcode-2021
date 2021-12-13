
module Main where

import qualified Data.Set as Set
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Util (inputRaw)


-- | Read the input file.
input :: String -> [([Set.Set Char], [Set.Set Char])]
input fileName = map parseLine $ lines $ inputRaw fileName

parseLine :: String -> ([Set.Set Char], [Set.Set Char])
parseLine line = (signal_patterns, digits_output)
    where
        [signal_patterns_, digits_output_] = splitOn " | " line
        signal_patterns = map Set.fromList $ splitOn " " signal_patterns_
        digits_output = map Set.fromList $ splitOn " " digits_output_


process :: [([Set.Set Char], [Set.Set Char])] -> Int
process ((signal_pattern, digits_outputs):other) =
    (length (filter (\x -> x `Set.member` Set.fromList [1,4,7,8]) digits)) + process other
    where
        digits = subProcess signal_pattern digits_outputs
process [] = 0

subProcess :: [Set.Set Char] -> [Set.Set Char] -> [Int]
subProcess signal_patterns digits_outputs = 
    map (\x -> fromMaybe 0 $ elemIndex x patterns) digits_outputs
    -- (patterns, digits_outputs)
    where
        one = head $ filter (\x -> Set.size x == 2) signal_patterns
        seven = head $ filter (\x -> Set.size x == 3) signal_patterns
        four = head $ filter (\x -> Set.size x == 4) signal_patterns
        eight = head $ filter (\x -> Set.size x == 7) signal_patterns
        nine = head $ filter (\x -> Set.size x == 6 && (Set.union four seven) `Set.isSubsetOf` x) signal_patterns
        five = head $ filter (\x -> Set.size x == 5 && (Set.difference nine seven) `Set.isSubsetOf` x) signal_patterns
        three = head $ filter (\x -> Set.size x == 5 && seven `Set.isSubsetOf` x) signal_patterns
        two = head $ filter (\x -> Set.size x == 5 && (Set.difference eight four) `Set.isSubsetOf` x) signal_patterns
        zero = head $ filter (\x -> Set.size x == 6 && (Set.difference eight five) `Set.isSubsetOf` x) signal_patterns
        six = head $ filter (\x -> Set.size x == 6 && (Set.difference two seven) `Set.isSubsetOf` x) signal_patterns
        patterns = [ zero
                   , one
                   , two
                   , three
                   , four
                   , five
                   , six
                   , seven
                   , eight
                   , nine]




main :: IO ()
main = printf "Day08 Part1: %s" $ show $ process $ input "inputs/08"