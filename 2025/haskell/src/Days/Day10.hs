{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.Day10 (day10a, day10b) where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.HashPSQ as H
import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Either (fromRight)
import Lib.Algorithm (dijkstra)
import Data.Maybe (fromJust, isNothing)

import Data.MemoTrie (memo)
import Data.List (subsequences)

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

part1 :: [Machine] -> Int
part1 = sum . map (\(ls, bs, _) -> myDijkstra (replicate (length ls) False) (getNeighbours' bs) (isEnd ls))

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

-- INFO: Endedup using this method: https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
-- This does not involve writing a solver nor using one. It only uses part1 logic and smartly brute forces part2.

type Buttons = [Int]
type Voltage = [Int]

day10b :: String -> String
day10b = show . part2 . parseLines . lines

part2 :: [Machine] -> Int
part2 = sum . map (\(_, btns, voltage) -> compute btns voltage)

compute :: [Buttons] -> Voltage -> Int
compute btns = cachedCompute
  where
    cachedCompute = memo go
    go voltage
      | isFinished voltage = 0
      | null possibleComb = 1_000_000
      | otherwise = minimum . map (\(v, c) -> 2 * cachedCompute v + c) $ voltageComb
      where
        possibleComb = getComb btns voltage
        voltages = map (`pressComb` voltage) possibleComb
        voltagesDiv2 = map (map (`div` 2)) voltages
        voltageComb = zipWith (\v comb -> (v, length comb)) voltagesDiv2 possibleComb

isFinished :: Voltage -> Bool
isFinished = all (== 0)

getComb :: [Buttons] -> Voltage -> [[Buttons]]
getComb btns voltage = [comb | comb <- subsequences btns, isValidComb comb voltage]

isValidComb :: [Buttons] -> Voltage -> Bool
isValidComb btns voltage = all (\v -> v >= 0 && even v) $ pressComb btns voltage

pressComb :: [Buttons] -> Voltage -> Voltage
pressComb btns voltage = zipWith (\ v i -> v - countOccur flatBtns i) voltage [0..]
  where
    flatBtns = concat btns

countOccur :: Buttons -> Int -> Int
countOccur btns i = length . filter (== i) $ btns
