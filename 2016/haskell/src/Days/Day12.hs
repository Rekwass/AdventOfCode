module Days.Day12 (day12a, day12b) where

{------------------------------{ 1st part }------------------------------}

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST (runST, ST)
import Control.Monad (when)

data Instruction x y = Cpy x y | Inc x | Dec x | Jnz x y deriving Show

type Instructions = V.Vector (Instruction String String)

type Registers = [Int]

day12a :: String -> String
day12a = show . part1 . parseLines . lines

parseLines :: [String] -> (Instructions, Registers)
parseLines xs = (V.fromList $ map (parseInstruction . words) xs
              , [0, 0, 0, 0])

parseInstruction :: [String] -> Instruction String String
parseInstruction ["cpy", x, y] = Cpy x y
parseInstruction ["jnz", x, y] = Jnz x y
parseInstruction ["inc", x]    = Inc x
parseInstruction ["dec", x]    = Dec x
parseInstruction _ = error "Impossible"

part1 :: (Instructions, Registers) -> Int
part1 (is, rs) = runST $ emulateST is rs

emulateST :: Instructions -> Registers -> ST s Int
emulateST is rs = do
  regs <- VM.new 4
  sequence_ [VM.write regs i v | (i, v) <- zip [0..] rs]
  pc <- VM.new 1
  VM.write pc 0 0
  let loop = do
        i <- VM.read pc 0
        when (i >= 0 && i < length is) $ do
          let instr = is `V.unsafeIndex` i
          case instr of
            Inc x -> inc regs x
            Dec x -> dec regs x
            Cpy x y -> cpy regs x y
            Jnz x y -> jnz regs pc x y
          VM.modify pc (+1) 0
          loop
  loop
  VM.read regs 0

inc :: (VM.PrimMonad m) => VM.MVector (VM.PrimState m) Int -> String -> m ()
inc regs x = do
              let idx = rToI x
              VM.modify regs (+1) idx

dec :: (VM.PrimMonad m) => VM.MVector (VM.PrimState m) Int -> String -> m ()
dec regs x = do
              let idx = rToI x
              VM.modify regs (subtract 1) idx

cpy :: (VM.PrimMonad m) => VM.MVector (VM.PrimState m) Int -> String -> String -> m ()
cpy regs x y = do
                let idxY = rToI y
                val <- if isRegister x
                         then VM.read regs (rToI x)
                         else return (read x)
                VM.write regs idxY val

jnz :: (VM.PrimMonad m) => VM.MVector (VM.PrimState m) Int -> VM.MVector (VM.PrimState m) Int -> String -> String -> m ()
jnz regs pc x y = do
                valX <- if isRegister x
                          then VM.read regs (rToI x)
                          else return (read x)
                when (valX /= 0) $ do
                  off <- if isRegister y
                           then VM.read regs (rToI y)
                           else return (read y)
                  VM.modify pc (+ (off - 1)) 0

isRegister :: String -> Bool
isRegister = flip elem ["a","b","c","d"]

rToI :: String -> Int
rToI r
  | r == "a" = 0
  | r == "b" = 1
  | r == "c" = 2
  | r == "d" = 3
  | otherwise = error (show r ++ " is not a register")

{------------------------------{ 2nd part }------------------------------}

day12b :: String -> String
day12b = show . part2 . parseLines' . lines

parseLines' :: [String] -> (Instructions, Registers)
parseLines' xs = (V.fromList $ map (parseInstruction . words) xs
              , [0, 0, 1, 0])

part2 :: (Instructions, Registers) -> Int
part2 (is, rs) = runST $ emulateST is rs
