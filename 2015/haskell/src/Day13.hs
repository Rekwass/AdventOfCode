{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day13 (day13a, day13b) where
import Data.Map (Map, empty, insertWith, keys, map, (!))
import Data.List.Split (splitOn)
import Data.List (permutations)

data Relation = Relation { neighbour :: String, happiness :: Int } deriving (Eq, Show)

day13a :: String -> String
day13a = show . run . parseLines . Prelude.map (splitOn " " . init) . lines

parseLines :: [[String]] -> Map String [Relation]
parseLines = foldr initRel empty
    where
      initRel [p,_,"gain",v,_,_,_,_,_,_,to] acc = insertWith (<>) p [Relation to (read v :: Int)] acc
      initRel [p,_,"lose",v,_,_,_,_,_,_,to] acc = insertWith (<>) p [Relation to ((negate . read) v :: Int)] acc

run :: Map String [Relation] -> Int
run m = computeRes m (permutations $ keys m)

computeRes ::  Map String [Relation] -> [[String]] -> Int
computeRes m = maximum . Prelude.map computeRes'
    where
      computeRes' xs = foldr (\x acc -> acc + leftOf x (cycle xs) m + rightOf x (cycle xs) m) 0 xs

leftOf :: String -> [String] -> Map String [Relation] -> Int
leftOf p (y:z:zs) m
    | p == z = getRelVal y $ m ! p 
    | otherwise = leftOf p (z:zs) m

rightOf :: String -> [String] -> Map String [Relation] -> Int
rightOf p (y:z:zs) m
    | p == y = getRelVal z $ m ! p
    | otherwise = rightOf p (z:zs) m

getRelVal :: String -> [Relation] -> Int
getRelVal to = foldr (\(Relation from v) acc -> if from == to then v else acc) 0

---------------------------------------------------------------------------
-- Part 2.

day13b :: String -> String
day13b = show . run . addMe . parseLines . Prelude.map (splitOn " " . init) . lines

addMe :: Map String [Relation] -> Map String [Relation]
addMe oldM = foldr insertMe updatedMap $ keys updatedMap
    where
      updatedMap = Data.Map.map (<> [Relation "Me" 0]) oldM
      insertMe to = insertWith (<>) "Me" [Relation to 0]
