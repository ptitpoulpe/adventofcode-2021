module Main where

import qualified Data.Map as Map
import Data.Char(digitToInt)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow, pTraceShowId)
import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)


data State =
    State { value :: Int
          , remaining :: [Int] } deriving (Show)

data OpState =
    OpState { values :: [Int]
            , remaining_ :: [Int]} deriving (Show)

-- | Read the input file.
input :: String -> [Int]
input fileName = bits
    where 
        bits = concatMap byte $ inputRaw fileName

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
process bits = value
    where
        (State value remaining) = parsePacket bits

parsePackets :: [Int] -> OpState
parsePackets bits
    | length bits > 6 = OpState (v:vs) rrbits
    where
        (State v rbits) = parsePacket bits
        (OpState vs rrbits) = parsePackets rbits
parsePackets bits = OpState [] bits
        

parsePacket :: [Int] -> State
parsePacket bits = po type_id tbits
    where
        (version, vbits) = splitAt 3 bits
        (type_id, tbits) = splitAt 3 vbits
        po [1, 0, 0] qbits = parseLiteral qbits
        po t qbits = 
            case t of
                [0, 0, 0] -> State (sum values) rbits
                [0, 0, 1] -> State (product values) rbits
                [0, 1, 0] -> State (minimum values) rbits
                [0, 1, 1] -> State (maximum values) rbits
                [1, 0, 1] -> State (if (values !! 0) > (values !! 1) then 1 else 0) rbits
                [1, 1, 0] -> State (if (values !! 0) < (values !! 1) then 1 else 0) rbits
                [1, 1, 1] -> State (if (values !! 0) == (values !! 1) then 1 else 0) rbits
                _ -> State 777 qbits
            where
                OpState values rbits = parseOperator qbits

parseLiteral :: [Int] -> State
parseLiteral bits = State (conv lbits) rbits
    where 
        (lbits, rbits) = parseLiteral' bits

parseLiteral' :: [Int] -> ([Int], [Int])
parseLiteral' (1:x:y:z:t:bits) = (x:y:z:t:sbits, rbits)
    where
        (sbits, rbits) = parseLiteral' bits
parseLiteral' (0:x:y:z:t:rbits) = ([x,y,z,t], rbits)
parseLiteral' _ = ([], [])

parseOperator :: [Int] -> OpState
parseOperator (0:bits) = OpState values rbits
    where
        (l, lbits) = splitAt 15 bits
        (nbits, rbits) = splitAt (conv l) lbits
        (OpState values _) = parsePackets nbits
parseOperator (1:bits) = OpState (reverse values) rbits
    where
        (l, lbits) = splitAt 11 bits
        f (OpState vs rs) x = OpState (v:vs) r
            where 
                (State v r) = parsePacket rs
        OpState values rbits = foldl f (OpState [] lbits) [1..conv l]
parseOperator _ = OpState [666] []

main :: IO ()
main = printf "Day16 Part2: %s" $ show $ process $ input "inputs/16"
