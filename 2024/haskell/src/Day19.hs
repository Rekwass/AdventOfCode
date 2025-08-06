{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day19 (day19a, day19b) where
import Data.List.Split (splitOn)
import Data.List (stripPrefix)
import Data.MemoTrie (memo)

data Onsen = Onsen {
  patterns :: [Pattern],
  towels :: [Towel]
  } deriving (Eq, Show)

type Pattern = String
type Towel = String

{------------------------------{ 1st part }------------------------------}

day19a :: String -> String
day19a =  show . part1 . parseLines . lines

parseLines :: [String] -> Onsen
parseLines (x:_:xs) = Onsen ps xs
  where
    ps = splitOn ", " x

part1 :: Onsen -> Int
part1 (Onsen ps ts) = length $ filter cachedFun ts
  where
    cachedFun = memo isPossible
    isPossible "" = True
    isPossible t = any (\p -> maybe False cachedFun (stripPrefix p t)) ps

{------------------------------{ 2nd part }------------------------------}

day19b :: String -> String
day19b =  show . part2 . parseLines . lines

part2 :: Onsen -> Int
part2 (Onsen ps ts) = foldr (\t count -> count + cachedFun t) 0 ts
  where
    cachedFun = memo isPossible
    isPossible "" = 1
    isPossible t = sum [ maybe 0 cachedFun (stripPrefix p t) | p <- ps]
