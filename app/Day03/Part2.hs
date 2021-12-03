module Main where

import Data.List (transpose, foldl')
import Text.Printf (printf)
import Util (inputRaw)

-- | Read the input file.
input :: String -> [[Int]]
input = map parse . lines . inputRaw
    where
        bit '0' = 0
        bit '1' = 1
        parse = map bit

--process :: [[Int]] -> Int
process lines = 
    (fromBinary $ filterLines 0 (>=0) lines) * (fromBinary $ filterLines 0 (<0) lines)
    where
        filterLines :: Int -> (Int -> Bool) -> [[Int]] -> [Int]
        filterLines _ _ [rline] = rline
        filterLines i func rlines =
            filterLines (i+1) func $ filter (\x -> x!!i == value) rlines
            where 
                sum_head = sum $ map (\x -> if x==1 then 1 else -1) $ (transpose rlines)!!i
                value = if func sum_head then 1 else 0
        fromBinary = foldl' (\acc b -> acc * 2 + b) 0

main :: IO ()
main = printf "Day03 Part2: %s" $ show $ process $ input "inputs/03"