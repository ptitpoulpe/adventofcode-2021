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
process :: [Int] -> [Board] -> [Int]
process (num:nums) boards = 
    (map (*num) valids_values) ++ (process nums unvalid_boards)
    where
        new_boards = map (map (map (\x -> if x == num then 0 else x))) boards
        (unvalid_boards, valids_values) = filterValid new_boards
process [] _ = []

filterValid :: [Board] -> ([Board], [Int])
filterValid (board:boards) = 
    case valid board of
        Nothing -> (board:unvalid_boards, valids_values)
        Just valid_value -> (unvalid_boards, valid_value:valids_values)
    where
        (unvalid_boards, valids_values) = filterValid boards
filterValid [] = ([], [])

valid :: Board -> Maybe Int
valid board =
    if rows || columns then Just sumCells else Nothing
    where 
        rows = any (\l -> sum l == 0) board
        columns = any (\l -> sum l == 0) $ transpose board
        sumCells = sum $ map sum board


main :: IO ()
main = printf "Day04 Part2: %s" $ show $ last $ process nums boards
    where
        (nums, boards) = input "inputs/04"