module Main where

import Text.Printf (printf)
import Util (inputRaw)

data Direction = Forward | Up | Down deriving (Enum, Show)

data Command =
    Command { direction :: Direction
            , distance :: Int } deriving (Show)

data Position =
    Position { x :: Int
             , y :: Int } deriving (Show)

-- | Read the input file.
input :: String -> [Command]
input = map parse . lines . inputRaw
    where 
        parse line = Command direction distance
            where
                direction =
                    case words line !! 0 of
                        "forward" -> Forward
                        "up" -> Up
                        "down" -> Down
                distance = read (words line !! 1)

process :: [Command] -> Int
process commands =
    x * y
    where
        Position x y = subProcess (Position 0 0) commands


subProcess :: Position -> [Command] -> Position
subProcess (Position x y) ((Command direction distance):commands) =
    case direction of
        Forward -> subProcess (Position (x + distance) y) commands
        Up -> subProcess (Position x (y - distance)) commands
        Down -> subProcess (Position x (y + distance)) commands
subProcess position [] = position

main :: IO ()
main = printf "Day02 Part1: %s" $ show $ process $ input "inputs/02"
