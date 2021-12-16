module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Char (isUpper)
import Data.List (intercalate, maximumBy, minimumBy)
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)


type Rules = Map.Map (Char, Char) Char
type PairCount = Map.Map (Char, Char) Int
type CharCount = Map.Map Char Int


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
        pairs = zip polymer $ tail polymer
        chars_count = Map.fromList $ Set.toList $ Set.map (\x -> (x, length $ filter (==x) polymer)) $ Set.fromList polymer
        pairs_count = Map.fromList $ Set.toList $ Set.map (\x -> (x, length $ filter (==x) pairs)) $ Set.fromList pairs
        sizes = Map.unionWith (+) chars_count  $ processPairs pairs_count 40 rules
        max = maximum $ Map.elems sizes
        min = minimum $ Map.elems sizes

processPairs :: PairCount -> Int -> Rules -> CharCount
processPairs _ 0 rules = Map.empty
processPairs pairs_count n rules = Map.unionWith (+) new_chars_incs $ processPairs new_pairs_count (n-1) rules
    where
        results = map (processPair pairs_count rules) $ Map.keys pairs_count
        chars_incs = concatMap fst results
        pairs_incs = concatMap snd results
        new_chars_incs = foldr (\(c, v) acc -> Map.insertWith (+) c v acc) Map.empty chars_incs
        new_pairs_count = foldr (\(p, v) acc -> Map.insertWith (+) p v acc) pairs_count pairs_incs

processPair :: PairCount -> Rules -> (Char, Char) -> ([(Char, Int)], [((Char, Char), Int)])
processPair pair_count rules p@(x,y)
    | p `Map.member` rules = (chars_incs, pairs_incs)
    where
        t = rules Map.! p
        c = pair_count Map.! p
        chars_incs = [(t, c)]
        pairs_incs = [((x,t),  c), ((t,y),  c), ((x,y), -c)]        
processPair _ _ _ = ([], [])

main :: IO ()
main = printf "Day14 Part2: %s" $ show $ process polymer rules
    where
        (polymer, rules) = input "inputs/14"