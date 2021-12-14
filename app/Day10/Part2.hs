module Main where

import Data.List (sort)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)

type Board = [[Int]]

data Result = COMPLETE | Incomplete String | Error Char | Error2 deriving (Show)

-- | Read the input file.
input :: String -> [String]
input = lines . inputRaw

process :: [String] -> Int
process chunks = scores!!(length scores `div` 2)
    where
        scoreChar '(' = 1
        scoreChar '[' = 2
        scoreChar '{' = 3
        scoreChar '<' = 4
        scoreChar _ = 0
        score (Incomplete stack) = foldl (\acc x -> 5 * acc + scoreChar x) 0 stack
        score _ = 0
        scores = sort $ filter (>0) $ map (\x -> score $ processLine x []) chunks

processLine :: String -> String -> Result
processLine (c:chunk) stack
    | c `elem` "([{<" = processLine chunk (c:stack)
processLine (c:chunk) (s:stack)
    | s == '(' && c == ')' = processLine chunk stack
    | s == '[' && c == ']' = processLine chunk stack
    | s == '{' && c == '}' = processLine chunk stack
    | s == '<' && c == '>' = processLine chunk stack
    | otherwise            = Error c
processLine _ "" = Error2
processLine "" stack = Incomplete stack

main :: IO ()
main = printf "Day10 Part2: %s" $ show $ process $ input "inputs/10"