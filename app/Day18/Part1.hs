module Main where

import qualified Data.Map as Map
import Data.Char(digitToInt)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe, isJust)
import Data.List.Split (splitOn)
import Data.List (group, sort)

data Tree =
    Tree { left :: Tree
           , right :: Tree } |
    Leaf { value :: Int } deriving (Show)


-- | Read the input file.
input :: String -> [Tree]
input fileName = map (fst . parseLine) $ lines $ inputRaw fileName


parseLine :: String -> (Tree, String)
parseLine ('[':rest) = (Tree left right, remainingr)
    where
        (left, ',':remainingl) = parseLine rest
        (right, ']':remainingr) = parseLine remainingl
parseLine (digit:rest) = (Leaf (digitToInt digit), rest)
parseLine _ = (Leaf 1000, "")

showTree :: Tree -> Tree
showTree tree = pTraceShow (printTree tree) tree 

printTree :: Tree -> String
printTree (Tree left right) = "[" ++ printTree left ++ "," ++ printTree right ++ "]"
printTree (Leaf value) = show value

-- | Process data
process :: [Tree] -> Int
process (tree:trees) = magnitude $ foldl (\acc t -> processTree $ Tree acc t) tree trees
process [] = -10000

processTree :: Tree -> Tree
processTree tree = first $ explode tree
    where
        second Nothing = tree
        second (Just jtree) = processTree jtree
        first Nothing = second $ split tree
        first (Just jtree) = processTree jtree

explode :: Tree -> Maybe Tree
explode tree = etree
    where
        (etree, _, _) = explode' tree 0


explode' :: Tree -> Int -> (Maybe Tree, Maybe Int, Maybe Int)
explode' (Tree (Leaf left) (Leaf right)) depth | depth >= 4 = (Just $ Leaf 0, Just left, Just right)
explode' (Tree left right) depth = case (ltree, rtree) of
    (Just ltree, _) -> (Just $ Tree ltree (applyRight lright right), lleft, Nothing)
    (Nothing, Just rtree) -> (Just $ Tree (applyLeft rleft left) rtree, Nothing, rright)
    _ -> (Nothing, Nothing, Nothing)
    where
        (ltree, lleft, lright) = explode' left (depth+1)
        (rtree, rleft, rright) = explode' right (depth+1)
explode' _ _ = (Nothing, Nothing, Nothing)

applyLeft :: Maybe Int -> Tree -> Tree
applyLeft Nothing tree = tree
applyLeft (Just x) (Leaf v) = Leaf (x+v)
applyLeft j (Tree left right) = Tree left (applyLeft j right)

applyRight :: Maybe Int -> Tree -> Tree
applyRight Nothing tree = tree
applyRight (Just x) (Leaf v) = Leaf (x+v)
applyRight j (Tree left right) = Tree (applyRight j left) right

split :: Tree -> Maybe Tree
split (Leaf v) | v > 9 = Just $ Tree (Leaf d) (Leaf (d + m))
    where
        (d,m) = v `divMod` 2
split (Tree left right) = case (ltree, rtree) of
    (Just ltree, _) -> Just $ Tree ltree right
    (Nothing, Just rtree) -> Just $ Tree left rtree
    _ -> Nothing
    where
        ltree = split left
        rtree = split right
split _ = Nothing

magnitude :: Tree -> Int
magnitude (Leaf v) = v
magnitude (Tree left right) = 3 * magnitude left + 2 * magnitude right

main :: IO ()
main = printf "Day18 Part1: %s" $ show $ process $ input "inputs/18" 
