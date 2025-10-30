module Lib.Cipher (caesar, vigenere) where

import Data.Char (chr, ord, isLower, isUpper)

-- >>> caesar 8 "caesar"
-- "kimaiz"
caesar :: Int -> String -> String
caesar n = map (`shiftLetter` n)

-- >>> shiftLetter 'c' 5
-- 'h'
shiftLetter :: Char -> Int -> Char
shiftLetter c n
    | isLower c = chr $ ((ord c - ord 'a' + n) `mod` 26) + ord 'a'
    | isUpper c = chr $ ((ord c - ord 'A' + n) `mod` 26) + ord 'A'
    | otherwise = c

-- >>> vigenere "secret message" "banana"
-- "teprrt nefsngf"
vigenere :: String -> String -> String
vigenere str key = go str $ cycle key
  where
    go []     _  = []
    go cs     [] = cs
    go (c:cs) kss@(k:ks)
      | isLower c = shiftLetter c (ord k - ord 'a') : go cs ks
      | isUpper c = shiftLetter c (ord k - ord 'A') : go cs ks
      | otherwise = c : go cs kss
