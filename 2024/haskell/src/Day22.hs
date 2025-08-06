module Day22 (day22a, day22b) where

import Data.Bits (Bits(xor))
import qualified Data.Map.Strict as M
import Data.List (foldl')

type Cache = M.Map (Int, Int, Int, Int) Int

{------------------------------{ 1st part }------------------------------}

day22a :: String -> String
day22a = show . part1 . parseLines

parseLines :: String -> [Int]
parseLines = map read . lines

part1 :: [Int] -> Int
part1 = sum . map ((!! 2000) . iterate monkeySecret)

monkeySecret :: Int -> Int
monkeySecret = stepThree . stepTwo . stepOne

stepOne :: Int -> Int
stepOne secret = prunedSecret
  where
    mul = secret * 64
    mixed = mix mul secret
    prunedSecret = prune mixed

stepTwo :: Int -> Int
stepTwo secret = prunedSecret
  where
    dived = secret `div` 32
    mixed = mix dived secret
    prunedSecret = prune mixed

stepThree :: Int -> Int
stepThree secret = prunedSecret
  where
    mul = secret * 2048
    mixed = mix mul secret
    prunedSecret = prune mixed

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = flip mod 16777216


{------------------------------{ 2nd part }------------------------------}

day22b :: String -> String
day22b =  show . part2 . parseLines

store :: Cache
store = M.empty

part2 :: [Int] -> Int
part2 = findMax . map computeAllSells

findMax :: [Cache] -> Int
findMax = maximum . M.elems . M.unionsWith (+)

computeAllSells :: Int -> Cache
computeAllSells x = foldl' addSell store digdiff
  where
    prices = take 2000 $ iterate monkeySecret x
    digits = map (`mod` 10) prices
    diffs = computeDiffs digits
    digdiff = combine digits diffs

computeDiffs :: [Int] -> [Int]
computeDiffs (x:y:xs) = y - x : computeDiffs (y:xs)
computeDiffs _        = []

combine :: [Int] -> [Int] -> [((Int, Int, Int, Int), Int)]
combine (_:xs@(_:_:_:v:_)) (a:ys@(b:c:d:_)) = ((a,b,c,d), v) : combine xs ys
combine _ _ = []

addSell :: Cache -> ((Int, Int, Int, Int), Int) -> Cache
addSell st (pattern, v)
  | pattern `M.notMember` st = M.insert pattern v st
  | otherwise                =  st
