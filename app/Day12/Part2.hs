
module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isUpper)
import Data.List.Split (splitOn)
import Text.Printf (printf)
import Util (inputRaw, pTraceShow)


data Node = Small String | Big String deriving (Show, Eq, Ord)

type Graph = Map.Map Node [Node]

-- | Read the input file.
input :: String -> Graph
input fileName = Map.fromListWith (++) $ concat $ map parseLine $ lines $ inputRaw fileName
    where
        parseLine line = [(node src, [node dst]), (node dst, [node src])] --[(node src, node dst), (node dst, node src)]
            where 
                [src, dst] = splitOn "-" line
                node s = if isUpper $ head s then Big s else Small s 

process :: Graph -> Int
process graph = Set.size $ Set.fromList $ map (unwords . map unpack) paths
    where 
        paths = subProcess (Small "start") False (Set.fromList [Small "start"]) graph
        unpack (Small s) = s
        unpack (Big s) = s

subProcess :: Node -> Bool -> Set.Set Node -> Graph -> [[Node]]
subProcess n@(Small "end") _ _ _ = [[n]]
subProcess node dup done graph = map (\ns -> node:ns) $ concat $ map doChild undoneChilds
    where
        undoneChilds = filter (`Set.notMember` done) (graph Map.! node)
        updateDone n@(Small _) = Set.insert n done
        updateDone (Big _) = done
        doChild n = case (dup, n) of
            (True, _) -> subProcess n True (updateDone n) graph
            (False, Big _) -> subProcess n False (updateDone n) graph
            (False, Small _) -> (subProcess n False (updateDone n) graph) ++ (subProcess n True done graph)
        

main :: IO ()
main = printf "Day12 Part2: %s" $ show $ process $ input "inputs/12"