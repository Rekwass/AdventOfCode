{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}

module Days.Day07 (day07a, day07b) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (foldl')

{------------------------------{ 1st part }------------------------------}

type Beams = S.Set Int
type Splitters = S.Set Int

day07a :: String -> String
day07a = show . part1 . parseLines . lines

parseLines :: [String] -> (Beams, [Splitters])
parseLines xs = (bs, ss)
  where
    bs = S.fromList [x | (x,'S') <- zip [0..] (head xs)]
    ss = map (\l -> S.fromList [x | (x,'^') <- zip [0..] l]) (tail xs)

part1 :: (Beams, [Splitters]) -> Int
part1 (bs, ss) = n
  where
    (_, n) = foldl' step (bs, 0) ss

step :: (Beams, Int) -> Splitters -> (Beams, Int)
step (bs, n) ss = (bs', n')
  where
    bsHit   = S.intersection bs ss
    bsSplit = S.foldr (\x -> S.insert (x-1) . S.insert (x+1)) S.empty bsHit
    bs'     = S.union bsSplit (bs `S.difference` bsHit)
    n'      = n + S.size bsHit

{------------------------------{ 2nd part }------------------------------}

type Beams' = M.Map Int Int

day07b :: String -> String
day07b = show . part2 . parseLines . lines

part2 :: (Beams, [Splitters]) -> Int
part2 (bs, ss) = M.foldr (+) 0 bm'
  where
    bm = M.fromList $ map (, 1) (S.toList bs)
    bm' = foldl' step' bm ss

step' :: Beams' -> Splitters -> Beams'
step' bm ss = bm'
  where
    bmHit   = M.filterWithKey (\k _ -> k `S.member` ss) bm
    bmSplit = M.foldrWithKey (\k x -> M.insertWith (+) (k-1) x . M.insertWith (+) (k+1) x) M.empty bmHit
    bm'     = M.unionWith (+) bmSplit (bm `M.difference` bmHit)
