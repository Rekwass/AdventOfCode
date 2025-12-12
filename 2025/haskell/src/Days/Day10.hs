{-# LANGUAGE OverloadedStrings #-}

module Days.Day10 (day10a, day10b) where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.HashPSQ as H
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Either (fromRight)
import Lib.Algorithm (dijkstra)
import Data.Maybe (fromJust, isNothing)

import Data.Hashable

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt = foldl hashWithSalt

{------------------------------{ 1st part }------------------------------}

type Machine = ([Bool], [[Int]], [Int])

day10a :: String -> String
day10a = show . part1 . parseLines . lines

parseLines :: [String] -> [Machine]
parseLines = map (fromRight undefined . parse machineParser "" . T.pack)

machineParser :: Parser Machine
machineParser = (,,)
  <$> lights
  <* many space
  <*> buttons `sepEndBy1` skipMany1 space
  <* many space
  <*> joltages

lights :: Parser [Bool]
lights = char '[' *> (map (== '#') <$> many1 (char '.' <|> char '#')) <* char ']'

buttons :: Parser [Int]
buttons = char '(' *> numberList <* char ')'

joltages :: Parser [Int]
joltages = char '{' *> numberList <* char '}'

numberList :: Parser [Int]
numberList = number `sepBy` char ','

number :: Parser Int
number = read <$> many1 digit

-- part1 :: [Machine] -> Int
part1 ms = sum $ aaa
  where
    aaa = map (\(ls, bs, _) -> myDijkstra (replicate (length ls) False) (getNeighbours' bs) (isEnd ls)) ms

getNeighbours' :: [[Int]] -> [Bool] -> [([Bool], Int)]
getNeighbours' bss ls = [(pressBtn bs ls, 1) | bs <- bss]

pressBtn :: [Int] -> [Bool] -> [Bool]
pressBtn bs ls = zipWith (\ l i -> (if i `elem` bs then not l else l)) ls [0..]

isEnd :: [Bool] -> [Bool] -> Bool
isEnd = (==)

myDijkstra :: [Bool] -> ([Bool] -> [([Bool], Int)]) -> ([Bool] -> Bool) -> Int
myDijkstra start getNeighbours isGoal
  | isNothing mPath = 0
  | otherwise = (+ (-1)) . length $ backtrace end
    where
      mPath = dijkstra queue dist prev getNeighbours isGoal
      path = fromJust mPath
      end = head . filter isGoal . M.keys $ path
      queue = H.singleton start 0 0
      dist = M.singleton start 0
      prev = M.empty
      backtrace = go
        where
          go current
            | current `M.notMember` path = [current]
            | otherwise = current : go (path M.! current)

{------------------------------{ 2nd part }------------------------------}

day10b :: String -> String
day10b = show . part2 . parseLines . lines

type Machine' = ([Bool], [[Int]], V.Vector Int)

-- part2 :: [String] -> Int
part2 ms = sum $ aaa
  where
    aaa = map (\(_, bs, js) -> myDijkstra' (V.fromList $ replicate (length js) 0) (getNeighbours'' js bs) (isEnd' js)) ms'
    ms' = toVec ms

toVec :: [Machine] -> [Machine']
toVec = map (\(ls, bs, js) -> (ls, bs, V.fromList js))

getNeighbours'' :: V.Vector Int -> [[Int]] -> V.Vector Int -> [(V.Vector Int, Int)]
getNeighbours'' og bss js = [res | bs <- bss, let res = (pressBtn' bs js, 1), isPossible og (fst res)]

isPossible :: V.Vector Int -> V.Vector Int -> Bool
isPossible og = all (\(l, r) -> r <= l) . V.zip og

pressBtn' :: [Int] -> V.Vector Int -> V.Vector Int
-- pressBtn' bs js = foldl' (press bs) [] (V.zip js $ V.fromList [0..])
-- pressBtn' bs js = V.foldl press js bs
pressBtn' bs js = js V.// [(b, (js V.! b) + 1) | b <- bs]

-- press :: V.Vector Int -> Int -> V.Vector Int
-- press acc i = acc V.// 
--   | i `elem` bs = acc ++ [j + 1]
--   | otherwise   = acc ++ [j]

isEnd' :: V.Vector Int -> V.Vector Int -> Bool
isEnd' = (==)

myDijkstra' :: V.Vector Int -> (V.Vector Int -> [(V.Vector Int, Int)]) -> (V.Vector Int -> Bool) -> Int
myDijkstra' start getNeighbours isGoal
  | isNothing mPath = 0
  | otherwise = (+ (-1)) . length $ backtrace end
    where
      mPath = dijkstra queue dist prev getNeighbours isGoal
      path = fromJust mPath
      end = head . filter isGoal . M.keys $ path
      queue = H.singleton start 0 0
      dist = M.singleton start 0
      prev = M.empty
      backtrace = go
        where
          go current
            | current `M.notMember` path = [current]
            | otherwise = current : go (path M.! current)
