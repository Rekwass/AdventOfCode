{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Days.Day21 (day21a, day21b) where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Either (fromRight)
import Data.Bifunctor (second)
import Data.Maybe (fromJust)
import Data.List (foldl', elemIndex)

{------------------------------{ 1st part }------------------------------}

data Instruction = SwapP Int Int
                 | SwapL Char Char
                 | RotateLeft Int
                 | RotateRight Int
                 | RotateLetter Char
                 | Reverse Int Int
                 | Move Int Int
                 deriving Show

day21a :: String -> String
day21a = show . part1 . parseLines . T.lines . T.pack

parseLines :: [T.Text] -> [Instruction]
parseLines = map (fromRight (error "impossible") . parse parseInstruction "")

parseInstruction :: Parser Instruction
parseInstruction = choice [
                      try parseSwapP
                    , try parseSwapL
                    , try parseRotateLeft
                    , try parseRotateRight
                    , try parseRotateLetter
                    , parseReverse
                    , parseMove]

parseSwapP :: Parser Instruction
parseSwapP = SwapP
  <$> (string "swap position " *> number)
  <*> (string " with position " *> number)

parseSwapL :: Parser Instruction
parseSwapL = SwapL
  <$> (string "swap letter " *> anyChar)
  <*> (string " with letter " *> anyChar)

parseRotateLeft :: Parser Instruction
parseRotateLeft = RotateLeft
  <$> (string "rotate " >> string "left " *> number)

parseRotateRight :: Parser Instruction
parseRotateRight = RotateRight
  <$> (string "rotate " >> string "right " *> number)

parseRotateLetter :: Parser Instruction
parseRotateLetter = RotateLetter
  <$> (string "rotate based on position of letter " *> anyChar)

parseReverse :: Parser Instruction
parseReverse = Reverse
  <$> (string "reverse positions " *> number)
  <*> (string " through " *> number)

parseMove :: Parser Instruction
parseMove = Move
  <$> (string "move position " *> number)
  <*> (string " to position " *> number)

number :: Parser Int
number = read <$> many1 digit

part1 :: [Instruction] -> String
part1 = foldl' (flip scramble) "abcdefgh"

scramble :: Instruction -> String -> String
scramble (SwapP        x  y)  = swapP x y
scramble (SwapL        c1 c2) = swapL c1 c2
scramble (RotateLeft   n)     = rotateLeft n
scramble (RotateRight  n)     = rotateRight n
scramble (RotateLetter c)     = rotateLetter c
scramble (Reverse      x y)   = rev x y
scramble (Move         x y)   = move x y

swapP :: Int -> Int -> String -> String
swapP x y s = b ++ [c2] ++ m ++ [c1] ++ a
  where
    x' = min x y
    y' = max x y
    c1 = s !! x'
    c2 = s !! y'
    (b, m) = second (tail . take (y' - x')) $ splitAt x' s
    a = drop (y' + 1) s

swapL :: Char -> Char -> String -> String
swapL c1 c2 s = b ++ [c2'] ++ m ++ [c1'] ++ a
  where
    x = fromJust $ elemIndex c1 s
    y = fromJust $ elemIndex c2 s
    x' = min x y
    y' = max x y
    c1' = s !! x'
    c2' = s !! y'
    (b, m) = second (tail . take (y' - x')) $ splitAt x' s
    a = drop (y' + 1) s

rotateLeft :: Int -> String -> String
rotateLeft n s = take n' . drop n . cycle $ s
  where
    n' = length s

rotateRight :: Int -> String -> String
rotateRight n s = take n' . drop r . cycle $ s
  where
    n' = length s
    r = n' - (n `mod` n')

rotateLetter :: Char -> String -> String
rotateLetter c s
  | i >= 4    = rotateRight (i + 2) s
  | otherwise = rotateRight (i + 1) s
  where
    i = fromJust $ elemIndex c s

rev :: Int -> Int -> String -> String
rev x y s = b ++ reverse m ++ a
  where
    x' = min x y
    y' = max x y
    (b, m) = second (take (y' - x' + 1)) $ splitAt x' s
    a = drop (y' + 1) s

move :: Int -> Int -> String -> String
move x y s = b ++ [c] ++ m
  where
    c = s !! x
    (b, m) = splitAt y $ filter (/= c) s

{------------------------------{ 2nd part }------------------------------}

day21b :: String -> String
day21b = show . part2 . parseLines . T.lines . T.pack

part2 :: [Instruction] -> String
part2 = foldr unScramble "fbgdceah"

unScramble :: Instruction -> String -> String
unScramble (SwapP        x  y)  = swapP x y
unScramble (SwapL        c1 c2) = swapL c1 c2
unScramble (RotateLeft   n)     = rotateRight n
unScramble (RotateRight  n)     = rotateLeft n
unScramble (RotateLetter c)     = unRotateLetter c
unScramble (Reverse      x y)   = rev x y
unScramble (Move         x y)   = unMove x y

unRotateLetter :: Char -> String -> String
unRotateLetter c s = rotateLeft i s
  where
    i = inverseRotateIdx . fromJust . elemIndex c $ s

inverseRotateIdx :: Int -> Int
inverseRotateIdx 0 = 1
inverseRotateIdx 1 = 1
inverseRotateIdx 2 = 6
inverseRotateIdx 3 = 2
inverseRotateIdx 4 = 7
inverseRotateIdx 5 = 3
inverseRotateIdx 6 = 0
inverseRotateIdx 7 = 4
inverseRotateIdx _ = error "impossible"

unMove :: Int -> Int -> String -> String
unMove x y s = b ++ [c] ++ m
  where
    c = s !! y
    (b, m) = splitAt x $ filter (/= c) s
