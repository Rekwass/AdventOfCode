{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day12 (day12a, day12b) where

import qualified Data.Map as M
import qualified Data.Set as S

type Map = M.Map Char (S.Set Point)
type Point = (Int, Int)
type Region = S.Set Point

{------------------------------{ 1st part }------------------------------}

day12a :: String -> String
day12a = show . part1 . parseLines . lines

parseLines :: [String] -> Map
parseLines = M.fromListWith S.union . concat . zipWith locatePlants [0..]
    where
      locatePlants y = zipWith (\x c -> (c, S.singleton (y, x))) [0..]

part1 :: Map -> Int
part1 m =
    let
      goodMap = M.map splitRegions m
      perimeters = M.map (map (length . getPerimeter) . S.toList) goodMap
      areas = M.map (map length . S.toList) goodMap
      prices = M.unionWith (zipWith (*)) perimeters areas
    in sum $ concatMap snd $ M.toList prices

splitRegions :: S.Set Point -> S.Set Region
splitRegions s
    | S.null s = S.empty
    | otherwise =
    let
      (l, r) = S.splitAt 1 s
      region = findRegion l r
      unvisited = S.difference s region
    in S.union (S.singleton region) $ splitRegions unvisited

findRegion :: S.Set Point -> S.Set Point -> Region
findRegion start xs = go start S.empty
    where
      go s visited
        | S.null s = visited
        | otherwise =
          let
            l = S.elemAt 0 s
            neighbours = S.filter (`isAdjacent` l) xs
            newNeighbours = S.filter (`S.notMember` visited) neighbours
          in go (S.union (S.drop 1 s) newNeighbours) (S.insert l visited)

getPerimeter :: Region -> [Point]
getPerimeter rss = go rss
    where
      go rs
        | S.null rs = []
        | otherwise =
          let
            (l, r) = S.splitAt 1 rs
            neighbours = S.filter (`S.notMember` rss) $ getNeighbours (S.elemAt 0 l)
          in S.toList neighbours ++ go r

getNeighbours :: Point -> S.Set Point
getNeighbours (y, x) = S.fromList [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]

isAdjacent :: Point -> Point -> Bool
isAdjacent (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2) == 1

{------------------------------{ 2nd part }------------------------------}

day12b :: String -> String
day12b = show . part2 . parseLines . lines

part2 :: Map -> Int
part2 m =
    let
      goodMap = M.map splitRegions m
      corners = M.map (map countCorners . S.toList) goodMap
      areas = M.map (map length . S.toList) goodMap
      prices = M.unionWith (zipWith (*)) corners areas
    in sum $ concatMap snd $ M.toList prices

countCorners :: Region -> Int
countCorners rss = go rss
    where
      go rs
        | S.null rs = 0
        | otherwise =
          let
            corners = getCorners rss (S.elemAt 0 rs)
          in length corners + go (S.drop 1 rs)

getCorners :: Region -> Point -> S.Set Point
getCorners rs (y, x) = S.fromList (tre ++ bre ++ ble ++ tle) `S.union` S.filter (`S.notMember` rs) (S.fromList $ trf ++ brf ++ blf ++ tlf)
    where
      tre = [(y - 1, x + 1) | pointsEmpty (y - 1, x) (y, x + 1) rs]
      bre = [(y + 1, x + 1) | pointsEmpty (y + 1, x) (y, x + 1) rs]
      ble = [(y + 1, x - 1) | pointsEmpty (y + 1, x) (y, x - 1) rs]
      tle = [(y - 1, x - 1) | pointsEmpty (y - 1, x) (y, x - 1) rs]
      trf = [(y - 1, x + 1) | pointsFull (y - 1, x) (y, x + 1) rs]
      brf = [(y + 1, x + 1) | pointsFull (y + 1, x) (y, x + 1) rs]
      blf = [(y + 1, x - 1) | pointsFull (y + 1, x) (y, x - 1) rs]
      tlf = [(y - 1, x - 1) | pointsFull (y - 1, x) (y, x - 1) rs]

pointsFull :: Point -> Point -> Region -> Bool
pointsFull p1 p2 r = p1 `S.member` r && p2 `S.member` r

pointsEmpty :: Point -> Point -> Region -> Bool
pointsEmpty p1 p2 r = p1 `S.notMember` r && p2 `S.notMember` r
