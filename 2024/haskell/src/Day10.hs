module Day10 (day10a, day10b) where

import qualified Data.Map as M

import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.List (nub)

type Map = M.Map Height [Point]
type Height = Int
type Point = (Int, Int)

{------------------------------{ 1st part }------------------------------}

day10a :: String -> String
day10a = show . part1 . parseLines . lines

parseLines :: [String] -> Map
parseLines = M.fromListWith (++) . concat . zipWith getPoints [0..]
    where
      getPoints y = zipWith (\x c -> (ord c - 48, [(y, x)])) [0..]

part1 :: Map -> Int
part1 m = sum $ map (length . nub . findSummits m 0 []) trailheads
    where
      trailheads = fromJust $ M.lookup 0 m

findSummits :: Map -> Height -> [Point] -> Point -> [Point]
findSummits _ 9 s p = p : s
findSummits m h s p = foldr (flip $ findSummits m (h + 1)) s $ neighbours m h p

neighbours :: Map -> Height -> Point -> [Point]
neighbours m h p = filter (isAdjacent p) pts
    where
      pts = fromJust $ M.lookup (h + 1) m

isAdjacent :: Point -> Point -> Bool
isAdjacent (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2) == 1

{------------------------------{ 2nd part }------------------------------}

day10b :: String -> String
day10b = show . part2 . parseLines . lines

part2 :: Map -> Int
part2 m = sum $ map (length . findSummits m 0 []) trailheads
    where
      trailheads = fromJust $ M.lookup 0 m
