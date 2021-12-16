module Main where

import qualified Data.Map as Map
import Data.Char(digitToInt)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)

data Done = Done [Int] [Int]
data State =
    State { versions :: [Int]
          , remaining :: [Int] } deriving (Show)

-- | Read the input file.
input :: String -> [Int]
input fileName = concatMap byte $ inputRaw fileName

byte '0' = [0,0,0,0]
byte '1' = [0,0,0,1]
byte '2' = [0,0,1,0]
byte '3' = [0,0,1,1]
byte '4' = [0,1,0,0]
byte '5' = [0,1,0,1]
byte '6' = [0,1,1,0]
byte '7' = [0,1,1,1]
byte '8' = [1,0,0,0]
byte '9' = [1,0,0,1]
byte 'A' = [1,0,1,0]
byte 'B' = [1,0,1,1]
byte 'C' = [1,1,0,0]
byte 'D' = [1,1,0,1]
byte 'E' = [1,1,1,0]
byte 'F' = [1,1,1,1]
byte _ = []

conv :: [Int] -> Int
conv = foldl (\acc b -> acc*2 + b) 0


process :: [Int] -> Int
process bits = sum versions
    where
        (State versions remaining) = parsePacket bits

parsePackets :: [Int] -> State
parsePackets bits
    | length bits > 6 = State (versions ++ oversions) orbits
    where
        State versions rbits = parsePacket bits
        State oversions orbits = parsePackets rbits
parsePackets bits = State [] bits
        

parsePacket :: [Int] -> State
parsePacket bits = State (conv version:versions) rbits
    where
        (version, vbits) = splitAt 3 bits
        (type_id, tbits) = splitAt 3 vbits
        State versions rbits = case type_id of
            [1, 0, 0] -> parseLiteral tbits
            _ -> parseOperator tbits

parseLiteral :: [Int] -> State
parseLiteral bits = State [] rbits
    where 
        (lbits, rbits) = parseLiteral' bits

parseLiteral' :: [Int] -> ([Int], [Int])
parseLiteral' (1:x:y:z:t:bits) = (x:y:z:t:sbits, rbits)
    where
        (sbits, rbits) = parseLiteral' bits
parseLiteral' (0:x:y:z:t:rbits) = ([x,y,z,t], rbits)
parseLiteral' _ = ([], [])

parseOperator :: [Int] -> State
parseOperator (0:bits) = State versions rbits
    where
        (l, lbits) = splitAt 15 bits
        (nbits, rbits) = splitAt (conv l) lbits
        s@(State versions r) = parsePackets nbits
parseOperator (1:bits) = s
    where
        (l, lbits) = splitAt 11 bits
        f (State v r) x = State (v++v') r'
            where 
                (State v' r') = parsePacket r
        s = foldl f (State [] lbits) [0..conv l]
parseOperator _ = State [] []

main :: IO ()
main = printf "Day16 Part1: %s" $ show $ process $ input $ "inputs/16" 
