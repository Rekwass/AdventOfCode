{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Day17 (day17a, day17b) where

import Data.List.Split (splitOn)
import Data.Bits (xor)
import Data.Maybe (catMaybes)

data Computer = Computer {
    a :: Register,
    b :: Register,
    c :: Register,
    ptr :: Int,
    instructions :: [Int]
  } deriving (Show)

type Register = Int

{------------------------------{ 1st part }------------------------------}

day17a :: String -> String
day17a = show . part1 . parseLines . lines

parseLines :: [String] -> Computer
parseLines [ra, rb, rc, _, prog] = Computer {a=aReg, b=bReg, c=cReg, ptr=0, instructions=istrs}
  where
    aReg = read . drop 12 $ ra
    bReg = read . drop 12 $ rb
    cReg = read . drop 12 $ rc
    istrs = map read . splitOn "," . drop 9 $ prog

part1 :: Computer -> [Int]
part1 computer = snd . runComputer computer $ []

runComputer :: Computer -> [Int] -> (Computer, [Int])
runComputer computer outs
      | computer.ptr >= length computer.instructions = (computer, outs)
      | otherwise = runComputer computer' outs'
      where
        opcode = computer.instructions !! computer.ptr
        operand = computer.instructions !! (computer.ptr + 1)
        (computer', outs') = runOpCode opcode (computer, outs) operand

combo :: Computer -> Int -> Int
combo _ 0 = 0
combo _ 1 = 1
combo _ 2 = 2
combo _ 3 = 3
combo computer 4 = computer.a
combo computer 5 = computer.b
combo computer 6 = computer.c
combo _ 7 = undefined

runOpCode :: Int -> (Computer, [Int]) -> Int -> (Computer, [Int])
runOpCode 0 = adv
runOpCode 1 = bxl
runOpCode 2 = bst
runOpCode 3 = jnz
runOpCode 4 = bxc
runOpCode 5 = out
runOpCode 6 = bdv
runOpCode 7 = cdv

-- 0
adv :: (Computer, [Int]) -> Int -> (Computer, [Int])
adv (computer, out) operand = (computer { a=newA, ptr=newPtr }, out)
  where
    comboOp = combo computer operand
    newA = computer.a `div` 2 ^ comboOp
    newPtr = computer.ptr + 2

-- 1
bxl :: (Computer, [Int]) -> Int -> (Computer, [Int])
bxl (computer, out) operand = (computer { b=newB, ptr=newPtr }, out)
  where
    newB = computer.b `xor` operand
    newPtr = computer.ptr + 2

-- 2
bst :: (Computer, [Int]) -> Int -> (Computer, [Int])
bst (computer, out) operand = (computer { b=newB, ptr=newPtr }, out)
  where
    comboOp = combo computer operand
    newB = comboOp `mod` 8
    newPtr = computer.ptr + 2

-- 3
jnz :: (Computer, [Int]) -> Int -> (Computer, [Int])
jnz (computer, out) operand
  | computer.a == 0 = (computer { ptr=newPtr }, out)
  | otherwise = (computer { ptr=operand }, out)
    where
    newPtr = computer.ptr + 2

-- 4
bxc :: (Computer, [Int]) -> Int -> (Computer, [Int])
bxc (computer, out) _ = (computer { b=newB, ptr=newPtr }, out)
  where
    newB= computer.b `xor` computer.c
    newPtr = computer.ptr + 2

-- 5
out :: (Computer, [Int]) -> Int -> (Computer, [Int])
out (computer, out) operand = (computer { ptr=newPtr }, out ++ [newY])
  where
    comboOp = combo computer operand
    newY = comboOp `mod` 8
    newPtr = computer.ptr + 2

-- 6
bdv :: (Computer, [Int]) -> Int -> (Computer, [Int])
bdv (computer, out) operand = (computer { b=newB, ptr=newPtr }, out)
  where
    comboOp = combo computer operand
    newB = computer.a `div` 2  ^ comboOp
    newPtr = computer.ptr + 2

-- 7
cdv :: (Computer, [Int]) -> Int -> (Computer, [Int])
cdv (computer, out) operand = (computer { c=newC, ptr=newPtr }, out)
  where
    comboOp = combo computer operand
    newC = computer.a `div` 2 ^ comboOp
    newPtr = computer.ptr + 2

{------------------------------{ 2nd part }------------------------------}

day17b :: String -> String
day17b =  show . part2 . parseLines . lines

myProgram :: Int -> Int
myProgram a =
  let
    b = a `mod` 8
    b' = b `xor` 4
    c = a `div` (2 ^ b')
    b'' = b' `xor` c
    b''' = b'' `xor` 4
    out = b''' `mod` 8
    -- a' = a `div` 8
  in out

part2 :: Computer -> Int
part2 computer = minimum . concatMap (catMaybes . findSmallest (reverse computer.instructions)) $ [1..7]

findSmallest :: [Int] -> Int -> [Maybe Int]
findSmallest [] _ = [Nothing]
findSmallest (x:xs) val 
  | myProgram val /= x = [Nothing]
  | null xs = [Just val]
  | otherwise = concatMap (\offset -> findSmallest xs (val * 8 + offset)) [0..7]
