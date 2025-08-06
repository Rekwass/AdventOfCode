module Day05 (day05a, day05b) where

import Data.List (isInfixOf, group)

day05a :: String -> String
day05a = show . length . part1 . lines

day05b :: String -> String
day05b = show . length . part2 . lines

part1 :: [String] -> [String]
part1 = filter (\x -> (length (filter (`elem` "aeiou") x) >= 3) && any ((>= 2) . length) (group x) && not (any (`isInfixOf` x) ["ab", "cd", "pq", "xy"]))

part2 :: [String] -> [String]
part2 = filter (\x -> hasDoublePair x && repeatingLetter x)

hasDoublePair :: String -> Bool
hasDoublePair []        = False
hasDoublePair (x:y:xs)
    | letterTwiceInARow x y xs = True
    | otherwise                = hasDoublePair (y:xs)
hasDoublePair _                = False

letterTwiceInARow :: Char -> Char -> String -> Bool
letterTwiceInARow _  _  []       = False
letterTwiceInARow c1 c2 (x:y:xs)
    | c1 == x && c2 == y         = True
    | otherwise                  = letterTwiceInARow c1 c2 (y:xs)
letterTwiceInARow _  _ _         = False

repeatingLetter :: String -> Bool
repeatingLetter []     = False
repeatingLetter (x:y:z:xs)
    | x == z = True
    | otherwise = repeatingLetter (y:z:xs)
repeatingLetter _ = False
