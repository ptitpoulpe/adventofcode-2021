module Main where

import qualified Data.Set as Set
import Data.Char (isUpper)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)


type Graph = Set.Set (Int, Int)

data Inst = FoldUp Int | FoldLeft Int deriving (Show)

-- | Read the input file.
input :: String -> (Graph, [Inst])
input fileName = (Set.fromList $ map parseDot $ lines dots, map parseInst $ lines insts)
    where
        [dots, insts] = splitOn "\n\n" $ inputRaw fileName

parseDot :: String -> (Int, Int)
parseDot s = (read x, read y)
    where
        [x, y] = splitOn "," s

parseInst :: String -> Inst
parseInst ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'x':'=':s) = FoldLeft $ read s
parseInst ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':'y':'=':s) = FoldUp $ read s

showG :: Graph -> Graph
showG graph = pTraceShow ( "\n" ++ intercalate "\n" [intercalate "" [if (x,y) `Set.member` graph then "#" else "." | x <- [0..width]] | y <- [0..height]]) graph
    where
        height = maximum $ Set.map (\(_,y) -> y) graph
        width = maximum $ Set.map (\(x,_) -> x) graph

process :: Graph -> [Inst] -> Int 
process graph (inst:_) = Set.size $ showG $ processG graph [inst]

processG :: Graph -> [Inst] -> Graph
processG graph ((FoldUp fy):instrs) = processG (Set.map (\(x, y) -> (x, if y < fy then y else (fy - (y - fy)))) graph) instrs
processG graph ((FoldLeft fx):instrs) = processG (Set.map (\(x, y) -> (if x < fx then x else (fx - (x - fx)), y)) graph) instrs
processG graph [] = graph

main :: IO ()
main = printf "Day13 Part1: %s" $ show $ process graph instrs
    where
        (graph, instrs) = input "inputs/13"