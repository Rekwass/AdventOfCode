{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}
{-# LANGUAGE QuasiQuotes #-}

module Days.Day06 (day06a, day06b) where

import Text.Regex.PCRE.Heavy (re, scan)
import Data.List (transpose)

{------------------------------{ 1st part }------------------------------}

data Operator = Add | Mul deriving (Eq, Show)

day06a :: String -> String
day06a = show . part1 . parseLines . lines

parseLines :: [String] -> [([Int], Operator)]
parseLines xs = zip ns' os'
  where
    (ns, [os]) = splitAt (length xs - 1) xs
    ns' = transpose [
      [read s | (s, _) <- scan [re|(\d+)|] l]
      | l <- ns
      ]
    os' = (map ((\x -> if x == "*" then Mul else Add) . fst) . scan [re|(\*)|(\+)|]) os

part1 :: [([Int], Operator)] -> Int
part1 = sum . map (\(n, o) -> foldr (op o) (base o) n)
  where
    base Add = 0
    base Mul = 1
    op Add = (+)
    op Mul = (*)

{------------------------------{ 2nd part }------------------------------}

day06b :: String -> String
day06b = show . part2 . parseLines' . lines

parseLines' :: [String] -> [([Int], Operator)]
parseLines' xs = zip ns' os'
  where
    (ns, [os]) = splitAt (length xs - 1) xs
    sns = map maximum . transpose . map (\l -> 
      [length s | (s,_) <- scan [re|(\d+)|] l]
      ) $ ns

    ns' = (map (map (read . removeSpaces) . transpose) . transpose . map (parseLine sns)) ns

    os' = (map ((\x -> if x == "*" then Mul else Add) . fst) . scan [re|(\*)|(\+)|]) os

parseLine :: [Int] -> String -> [String]
parseLine []     _ = []
parseLine (x:xs) y = chunk : parseLine xs rest
  where
    chunk = take x y
    rest = drop (x + 1) y

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

part2 :: [([Int], Operator)] -> Int
part2 = sum . map (\(n, o) -> foldr (op o) (base o) n)
  where
    base Add = 0
    base Mul = 1
    op Add = (+)
    op Mul = (*)
