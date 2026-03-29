{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.Day18 (day18a, day18b) where

{------------------------------{ 1st part }------------------------------}

type Tile = Bool

day18a :: String -> String
day18a = show . part1 . parseLines . lines

parseLines :: [String] -> [Bool]
parseLines = map convert . head
  where
    convert '.' = True
    convert '^' = False

part1 :: [Tile] -> Int
part1 = length . concatMap (filter id) . take 40 . iterate genNextRow

genNextRow :: [Tile] -> [Tile]
genNextRow ts = map isSafe groups
  where
    ts' = True : ts ++ [True]
    groups = groupBy3 ts'

groupBy3 :: [Tile] -> [(Tile, Tile, Tile)]
groupBy3 (x:y:z:zs) = (x,y,z) : groupBy3 (y:z:zs)
groupBy3 _          = []

isSafe :: (Tile, Tile, Tile) -> Tile
isSafe (l, m, r)
  | not l && not m && r = False
  | not m && not r && l = False
  | not l && m     && r = False
  | not r && m     && l = False
  | otherwise           = True

{------------------------------{ 2nd part }------------------------------}

day18b :: String -> String
day18b = show . part2 . parseLines . lines

part2 :: [Tile] -> Int
part2 = length . concatMap (filter id) . take 400000 . iterate genNextRow
