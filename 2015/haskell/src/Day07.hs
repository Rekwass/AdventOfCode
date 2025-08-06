module Day07 (day07a, day07b) where

import Data.List.Split (splitOn)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, complement)
import Data.Word (Word16)
import Data.Char (isAlpha)

data Signal = Signal String Word16 deriving (Eq, Show)

day07a :: String -> String
day07a = show . (`getValFromVar` "a") . run [] . parseLine . lines

parseLine :: [String] -> [[String]]
parseLine = map (splitOn " ")

run :: [Signal] -> [[String]] -> [Signal]
run  xs [] = xs
run xs (y:ys) = if canRunInstruction xs y then run (runInstruction xs y) ys else run xs (ys ++ [y])

canRunInstruction :: [Signal] -> [String] -> Bool
canRunInstruction xs [y1,['-','>'],_] = varInSignal xs y1
canRunInstruction xs [y1,['A','N','D'],y2,_,_] = varInSignal xs y1 && varInSignal xs y2
canRunInstruction xs [y1,['O','R'],y2,_,_] =  varInSignal xs y1 && varInSignal xs y2
canRunInstruction xs [y1,['L','S','H','I','F', 'T'],_,_,_] = varInSignal xs y1
canRunInstruction xs [y1,['R','S','H','I','F', 'T'],_,_,_] = varInSignal xs y1
canRunInstruction xs [['N','O','T'],y1,_,_] = varInSignal xs y1
canRunInstruction _ _ = error "Invalid instruction"

runInstruction :: [Signal] -> [String] -> [Signal]
runInstruction xs [y1,['-','>'],var] = Signal var (getValFromVar xs y1) : xs
runInstruction xs [y1,['A','N','D'],y2,_,var] = Signal var ((.&.) (getValFromVar xs y1) (getValFromVar xs y2)) : xs
runInstruction xs [y1,['O','R'],y2,_,var] = Signal var ((.|.) (getValFromVar xs y1) (getValFromVar xs y2)) : xs
runInstruction xs [y1,['L','S','H','I','F', 'T'],val,_,var] = Signal var (shiftL (getValFromVar xs y1) (read val)) : xs
runInstruction xs [y1,['R','S','H','I','F', 'T'],val,_,var] = Signal var (shiftR (getValFromVar xs y1) (read val)) : xs
runInstruction xs [['N','O','T'],y1,_,var] = Signal var (complement (getValFromVar xs y1)) : xs
runInstruction _ _ = error "Invalid instruction"

getValFromVar :: [Signal] -> String -> Word16
getValFromVar [] ys = read ys
getValFromVar (Signal var val:xs) ys = if var == ys then val else getValFromVar xs ys

varInSignal :: [Signal] -> String -> Bool
varInSignal xs v = not (isVar v) || any (\(Signal var _) -> var == v) xs

isVar :: String -> Bool
isVar = all isAlpha

---------------------------------------------------------------------------
-- Part 2.

day07b :: String -> String
day07b = show . (`getValFromVar` "a") . run' [] . parseLine . lines

run' :: [Signal] -> [[String]] -> [Signal]
run'  xs [] = xs
run' xs (y:ys) = if canRunInstruction xs y then run' (runInstruction' xs y) ys else run' xs (ys ++ [y])

runInstruction' :: [Signal] -> [String] -> [Signal]
runInstruction' xs [_,['-','>'],['b']] = Signal "b" 956 : xs -- INFO: 956 is the value of `a` in part 1
runInstruction' xs [y1,['-','>'],var] = Signal var (getValFromVar xs y1) : xs
runInstruction' xs [y1,['A','N','D'],y2,_,var] = Signal var ((.&.) (getValFromVar xs y1) (getValFromVar xs y2)) : xs
runInstruction' xs [y1,['O','R'],y2,_,var] = Signal var ((.|.) (getValFromVar xs y1) (getValFromVar xs y2)) : xs
runInstruction' xs [y1,['L','S','H','I','F', 'T'],val,_,var] = Signal var (shiftL (getValFromVar xs y1) (read val)) : xs
runInstruction' xs [y1,['R','S','H','I','F', 'T'],val,_,var] = Signal var (shiftR (getValFromVar xs y1) (read val)) : xs
runInstruction' xs [['N','O','T'],y1,_,var] = Signal var (complement (getValFromVar xs y1)) : xs
runInstruction' _ _ = error "Invalid instruction"
