module Day23 (day23a, day23b) where
import Data.List.Split (splitOn)

{------------------------------{ 1st part }------------------------------}

type Param = String
type Offset = Int

data Instruction = Hlf      Param
                 | Tpl      Param
                 | Inc      Param
                 | Jump     Offset
                 | JumpEven Param  Offset
                 | JumpOne  Param  Offset deriving (Eq, Show)

day23a :: String -> String
day23a = show . part1 . parseLines . lines

parseLines :: [String] -> [Instruction]
parseLines = map (parseLine . splitOn " ")

parseLine :: [String] -> Instruction
parseLine [['h', 'l', 'f'], r]       = Hlf r
parseLine [['t', 'p', 'l'], r]       = Tpl r
parseLine [['i', 'n', 'c'], r]       = Inc r
parseLine [['j', 'm', 'p'], s:nb]    = Jump $ parseNum s nb
parseLine [['j', 'i', 'e'], r, s:nb] = JumpEven (init r) $ parseNum s nb
parseLine [['j', 'i', 'o'], r, s:nb] = JumpOne  (init r) $ parseNum s nb
parseLine _ = error "How ???"

parseNum :: Char -> String -> Offset
parseNum '-' = negate . read
parseNum _   = read

part1 :: [Instruction] -> Int
part1 = runProgram 0 0 0

runProgram :: Int -> Int -> Int -> [Instruction] -> Int
runProgram a b idx is
  | isOOB idx is = b
  | otherwise    = runProgram a' b' idx' is
  where
    (a', b', idx') = runInstruction a b idx $ is !! idx

isOOB :: Int -> [Instruction] -> Bool
isOOB idx is = idx < 0 || idx >= length is

runInstruction :: Int -> Int -> Int -> Instruction -> (Int, Int, Int)
runInstruction a b idx (Hlf      ['a']) = (hlf a, b, inc idx)
runInstruction a b idx (Hlf      ['b']) = (a,     hlf b, inc idx)
runInstruction a b idx (Tpl      ['a']) = (tpl a, b,     inc idx)
runInstruction a b idx (Tpl      ['b']) = (a,     tpl b, inc idx)
runInstruction a b idx (Inc      ['a']) = (inc a, b,     inc idx)
runInstruction a b idx (Inc      ['b']) = (a,     inc b, inc idx)
runInstruction a b idx (Jump     p)     = (a,    b,      idx + p)
runInstruction a b idx (JumpEven p o)
  | (p == "a" && even a) || (p == "b" && even b) = (a, b, idx + o)
  | otherwise                                    = (a, b, inc idx)
runInstruction a b idx (JumpOne  p o)
  | (p == "a" && a == 1) || (p == "b" && b == 1) = (a, b, idx + o)
  | otherwise                                    = (a, b, inc idx)
runInstruction _ _ _ _ = error "How ???"

hlf :: Int -> Int
hlf = flip div 2

tpl :: Int -> Int
tpl = (*) 3

inc :: Int -> Int
inc = (+) 1

{------------------------------{ 2nd part }------------------------------}

day23b :: String -> String
day23b = show . part2 . parseLines . lines

part2 :: [Instruction] -> Int
part2 = runProgram 1 0 0
