module Day04 (day04a, day04b) where

import Data.Hash.MD5
import Data.List (isPrefixOf)

day04a :: String -> String
day04a xs = filter (not . (`elem` xs)) $ getHash [ x | x <- xs, x `notElem ` "\n"] "00000"

day04b :: String -> String
day04b xs = filter (not . (`elem` xs)) $ getHash [ x | x <- xs, x `notElem ` "\n"] "000000"

getHash :: String -> String -> String
getHash x prefix = head $ dropWhile (not . isPrefixOf prefix . md5s . Str) $ map ((x ++) . show) [0 ..]
