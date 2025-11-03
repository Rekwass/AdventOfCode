{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Days.Day05 (day05a, day05b) where

import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString.Char8 as B
import Data.List (isPrefixOf, scanl')
import Data.Char (digitToInt)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))

{------------------------------{ 1st part }------------------------------}

day05a :: String -> String
day05a = show . part1 . head . lines

part1 :: String -> String
part1 i = take 8 [hex !! 5 | n <- [0..]
                    , let bs = B.pack (i ++ show n)
                          digest = hash bs
                          hex =  B.unpack (convertToBase Base16 digest)
                    , "00000" `isPrefixOf` hex]

{------------------------------{ 2nd part }------------------------------}

day05b :: String -> String
day05b = show . part2 . head . lines

part2 :: String -> String
part2 i = head $ dropWhile (elem '_') $ scanl' replaceOrSkip (replicate 8 '_') hashes
  where
    hashes = [(digitToInt pos, hex !! 6) | n <- [0..]
                    , let bs = B.pack (i ++ show n)
                          digest = hash bs
                          hex =  B.unpack (convertToBase Base16 digest)
                     , "00000" `isPrefixOf` hex
                     , let pos = hex !! 5
                     , pos `elem` "01234567"
                     ]

replaceOrSkip :: String -> (Int, Char) -> String
replaceOrSkip s (p, c)
  | s !! p == '_' = replaceAt p c s
  | otherwise     = s

replaceAt :: Int -> Char -> String -> String
replaceAt n c s = take n s ++ [c] ++ drop (n + 1) s
