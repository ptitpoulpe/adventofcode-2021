module Main where

import Text.Parsec
import Data.List (transpose, foldl')
import Text.Printf (printf)
import Util (inputRaw)

type Board = [[Int]]

-- | Read the input file.
input :: String -> ([Int], [Board])
input fileName = do
    let (Right (nums, boards)) = parse parser "" (inputRaw fileName)
    (nums, boards)

parser :: Parsec String () ([Int],[Board])
parser = do
    nums <- number `sepBy` char ','
    newline
    newline
    boards <- parseBoard `sepBy1` newline
    return (nums, boards)

number :: Parsec String () Int
number = read <$> many1 digit

parseBoard :: Parsec String () Board
parseBoard = many1 $ do
    nums <- try (do
               many spaceChar
               xs <- number `sepEndBy1` (many1 spaceChar)
               newline
               return xs)
    return nums

spaceChar = char ' '


-- | Process Data
process :: [Int] -> [Board] -> Int
process (num:nums) boards = 
    case firstValid new_boards of 
        Just x -> x * num
        Nothing -> process nums new_boards
    where
        new_boards = map (map (map (\x -> if x == num then 0 else x))) boards
        valid_board = firstValid new_boards
process [] _ = 0

firstValid :: [Board] -> Maybe Int
firstValid (board:boards) = 
    case valid board of
        Nothing -> firstValid boards
        x -> x
firstValid _ = Nothing

valid :: Board -> Maybe Int
valid board =
    if rows || columns then Just sumCells else Nothing
    where 
        rows = any (\l -> sum l == 0) board
        columns = any (\l -> sum l == 0) $ transpose board
        sumCells = sum $ map sum board


main :: IO ()
main = printf "Day03 Part1: %s" $ show $ process nums boards
    where
        (nums, boards) = input "inputs/04"