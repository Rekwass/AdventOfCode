{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Day18 (day18a, day18b) where

import qualified Data.Set as S
import qualified Data.HashPSQ as H
import qualified Data.Map.Strict as M
import Data.Hashable (Hashable)
import Linear (V2 (V2), R1 (_x), R2 (_y))
import Control.Lens ((^.))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe, isJust, isNothing)

data Maze = Maze {
        width :: Int,
        walls :: [Point],
        start :: Point,
        end :: Point
    } deriving (Show)

type Point = V2 Int
type Cost = Int

{------------------------------{ 1st part }------------------------------}

day18a :: String -> String
day18a =  show . part1 . parseLines . lines

parseLines :: [String] -> Maze
parseLines ls = Maze {width=maxWidth + 1, walls=ws, start=start, end=end}
  where
    ws = getWalls ls
    maxWidth = maximum (map (^. _x) ws)
    maxHeight = maximum (map (^. _y) ws)
    start = V2 0 0
    end = V2 maxWidth maxHeight

getWalls :: [String] -> [Point]
getWalls [] = []
getWalls (l:ls) = V2 (read y) (read x) : getWalls ls
  where
    [y, x] = splitOn "," l

part1 :: Maze -> Int
part1 maze = S.size path - 1
-- part1 maze = displayMaze maze.walls maze.width path
  where
    path = fromJust . myDijkstra maze.start (getNeighbours' (S.fromList . take 1024 $ maze.walls) maze.width) . isGoal' $ maze.end

dirs :: [Point]
dirs = [V2 (-1) 0, V2 0 1, V2 1 0, V2 0 (-1)]

getNeighbours' :: S.Set Point -> Int -> Point -> [(Point, Cost)]
getNeighbours' ws width p = filter (not . oob . fst) . filter (flip S.notMember ws . fst) $ neighbours
  where
    neighbours = map ((, 1) . (+) p) dirs
    oob (V2 y x) = y < 0 || x < 0 || y >= width || x >= width

isGoal' :: Point -> Point -> Bool
isGoal' = (==)

-- displayMaze :: [Point] -> Int -> S.Set Point -> String
-- displayMaze ws width path = intercalate "\n" [line y | y <- [0..width-1]]
--   where
--     line y = [addChar (V2 x y) | x <- [0..width-1]]
--     addChar p
--       | p `S.member` path = 'O'
--       | p `elem` ws = '#'
--       | otherwise = '.'

myDijkstra :: (Num c, Ord c, Hashable v, Ord v) => v -> (v -> [(v, c)]) -> (v -> Bool) -> Maybe (S.Set v)
myDijkstra start getNeighbours isGoal
  | isNothing mPath = Nothing 
  | otherwise = Just (backtrace ends)
    where
      mPath = dijkstra queue dist prev getNeighbours isGoal
      path = fromJust mPath
      queue = H.singleton start 0 0
      dist = M.singleton start 0
      prev = M.empty 
      ends = head . filter isGoal . M.keys $ path
      backtrace = go
        where
          go current
            | current `M.notMember` path = S.singleton current
            | otherwise = S.insert current (go $ path M.! current)

dijkstra :: (Num c, Ord c, Hashable v, Ord v) => H.HashPSQ v c c -> M.Map v c -> M.Map v v -> (v -> [(v, c)]) -> (v -> Bool) -> Maybe (M.Map v v)
dijkstra queue dist prev getNeighbours isGoal
    | H.null queue = Nothing
    | isGoal current = Just prev
    | otherwise = dijkstra queue' dist' prev' getNeighbours isGoal
    where
      (current, _, distStart, rest) = fromJust . H.minView $ queue
      ngbrs = mapMaybe consider . getNeighbours $ current
      queue' = foldr (\(nbr, cost) -> H.insert nbr cost cost) rest ngbrs
      dist' = foldr (uncurry M.insert) dist ngbrs
      prev' = foldr (\(ngbr, _) -> M.insert ngbr current) prev ngbrs
      consider (ngb, dstNgbr)
        | ngb `M.notMember` dist || newDist < dist M.! ngb = Just (ngb, newDist)
        | otherwise = Nothing
        where
          newDist = distStart + dstNgbr

{------------------------------{ 2nd part }------------------------------}

day18b :: String -> String
day18b =  show . part2 . parseLines . lines

part2 :: Maze -> Point
part2 maze = maze.walls !! binarySearch (\n -> myDijkstra maze.start (getNeighbours'' n) . isGoal' $ maze.end) 1025 (length maze.walls)
  where
    getNeighbours'' n = getNeighbours' (S.fromList . take n $ maze.walls) maze.width

binarySearch :: (Int -> Maybe (S.Set v)) -> Int -> Int -> Int
binarySearch dijk low high
  | high < low = -1
  | isJust midPath && isNothing midPath' = mid
  | isNothing midPath = binarySearch dijk low (mid - 1)
  | otherwise = binarySearch dijk (mid + 1) high
  where
    midPath = dijk mid
    midPath' = dijk (mid + 1)
    mid = low + (high - low) `div` 2
