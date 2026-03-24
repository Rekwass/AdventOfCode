{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.Day14 (day14a, day14b) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import Data.List (group)

{------------------------------{ 1st part }------------------------------}

day14a :: String -> String
day14a = show . part1 . head . lines

part1 :: String -> Int
part1 salt = fst $ filter (\(idx, h) -> is3 h && is5 (take 1000 $ drop (idx + 1) hs) h) (zip [0..] hs) !! 63
  where
    hs = genAllHashes salt

genAllHashes :: String -> [String]
genAllHashes salt = [ B8.unpack
                    . B16.encode
                    . MD5.hash
                    . B8.pack
                    $ salt ++ show i | i <- [(0 :: Int)..]]

is3 :: String -> Bool
is3 = any ((>= 3) . length) . group

is5 :: [String] -> String -> Bool
is5 hs h = any (any (c `elem`) . filter ((>= 5) . length) . group) hs
  where
    c = head . head . filter ((>= 3) . length) $ group h

{------------------------------{ 2nd part }------------------------------}

day14b :: String -> String
day14b = show . part2 . head . lines

part2 :: String -> Int
part2 salt = fst $ filter (\(idx, h) -> is3 h && is5 (take 1000 $ drop (idx + 1) hs) h) (zip [0..] hs) !! 63
  where
    hs = genAllHashes' salt

genAllHashes' :: String -> [String]
genAllHashes' salt = [B8.unpack . (!! 2017) . iterate (
                      B16.encode
                     . MD5.hash
                   ) . B8.pack
                    $ salt ++ show i | i <- [(0 :: Int)..]]
