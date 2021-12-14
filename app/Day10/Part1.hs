module Main where

import Data.List (transpose, foldl')
import Text.Printf (printf)
import Util (inputRaw)

type Board = [[Int]]

data Result = COMPLETE | Incomplete | Error Char | Error2 deriving (Show)

-- | Read the input file.
input :: String -> [String]
input = lines . inputRaw

process :: [String] -> Int
process chunks = sum $ map (\x -> score $ processLine x []) chunks
    where
        score (Error ')') = 3
        score (Error ']') = 57
        score (Error '}') = 1197
        score (Error '>') = 25137
        score _ = 0

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
processLine "" _ = Incomplete

main :: IO ()
main = printf "Day10 Part1: %s" $ show $ process $ input "inputs/10"