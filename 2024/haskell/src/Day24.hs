module Day24 (day24a, day24b) where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe, isNothing, fromJust, catMaybes)
import Data.List (sortBy)
import Text.Printf (printf)

{------------------------------{ 1st part }------------------------------}

type State = Int

type Wires = M.Map String State

type Network = M.Map String (String, String, String)

day24a :: String -> String
day24a = show . part1 . parseLines

parseLines :: String -> (Wires, Network)
parseLines = go . splitOn [""] . lines
  where
    go [ws, is] = (parseWires ws, parseInstructions is)
    go _ = error "???"

parseWires :: [String] -> Wires
parseWires = foldr go M.empty
  where
    go [x,y,z,_,_,n] = M.insert [x,y,z] $ digitToInt n
    go _ = undefined

parseInstructions :: [String] -> Network
parseInstructions = foldr go M.empty
  where
    go [a,b,c,_,'A','N','D',_,d,e,f,_,_,_,_,g,h,i] = M.insert [g,h,i] ([a,b,c], "AND", [d,e,f])
    go [a,b,c,_,'X','O','R',_,d,e,f,_,_,_,_,g,h,i] = M.insert [g,h,i] ([a,b,c], "XOR", [d,e,f])
    go [a,b,c,_,'O','R',_,d,e,f,_,_,_,_,g,h,i] = M.insert [g,h,i] ([a,b,c], "OR", [d,e,f])
    go _ = undefined

part1 :: (Wires, Network) -> Int
part1 (ws, nw) = str
  where
    res = map go $ getAllZ nw
    go v = (v, computeValue v ws nw)
    str = createString res

computeValue :: String -> Wires -> Network -> State
computeValue v ws nw
  | op == "AND" = andGate
  | op == "OR" = orGate
  | otherwise = xorGate
  where
    (l, op, r) = nw M.! v
    lValM = M.lookup l ws
    rValM = M.lookup r ws
    lVal = fromMaybe (computeValue l ws nw) lValM
    rVal = fromMaybe (computeValue r ws nw) rValM
    andGate :: State
    andGate
      | lVal == 0 || rVal == 0 = 0
      | lVal == 1 && rVal == 1 = 1
      | otherwise = computeValue v ws nw
    orGate
      | lVal == 1 || rVal == 1 = 1
      | lVal == 0 && rVal == 0 = 0
      | otherwise = computeValue v ws nw
    xorGate
      | (lVal == 0 && rVal == 1) || (lVal == 1 && rVal == 0) = 1
      | (lVal == 1 && rVal == 1) || (lVal == 0 && rVal == 0) = 0
      | otherwise = computeValue v ws nw

getAllZ :: Network -> [String]
getAllZ nw = filter startWithZ $ M.keys nw
  where
    startWithZ ('z':_) = True
    startWithZ _       = False

createString :: [(String, State)] -> Int
createString xs = binaryToDecimal onlyState
  where
    onlyState = map snd sorted
    sorted = sortBy sortThem xs
    sortThem (a, _) (b, _) = compare a b

binaryToDecimal :: [State] -> Int
binaryToDecimal = foldr (\x acc -> acc * 2 + x) 0

{------------------------------{ 2nd part }------------------------------}

{-

Half adder

    (X)----─+-─:::::::::
            |  :: XOR ::--(Z)
    (Y)--+--|--:::::::::
         |  |                    
         |  +--:::::::::         
         |     :: AND ::--(C)
         +----─:::::::::

Full adder

    (X)----─+-─:::::::::
            |  :: XOR ::--(M)-----+----─:::::::::
    (Y)--+--|--:::::::::          |     :: XOR ::-----------------(Z)
         |  |                     |  +--:::::::::
         |  +--:::::::::          |  |
         |     :: AND ::--(N)--+  +--|--:::::::::
         +----─:::::::::       |     |  :: AND ::--(R)--::::::::
  (Cin)------------------------|-----+--:::::::::       :: OR ::--(Cout)
                               +------------------------::::::::

{------------------------------{ Manual parsing ... }------------------------------}

    (X06)----─+-─:::::::::
              |  :: XOR ::--(vkd)-------+----─:::::::::                   v
    (Y06)--+--|--:::::::::              |     :: XOR ::-----------------(fhc)
           |  |                         |  +--:::::::::
           |  +--:::::::::              |  |
           |     :: AND ::--(jnt)----+  +--|--:::::::::
           +----─:::::::::           |     |  :: AND ::--(vsv)--::::::::    v
  (scp)------------------------------|-----+--:::::::::         :: OR ::--(z06)
                                     +--------------------------::::::::

--------------------------------------------------

    (X11)----─+-─:::::::::
              |  :: XOR ::--(nmm)-------+----─:::::::::                   v
    (Y11)--+--|--:::::::::              |     :: XOR ::-----------------(qhj)
           |  |                         |  +--:::::::::
           |  +--:::::::::              |  |
           |     :: AND ::--(ghk)----+  +--|--:::::::::    v
           +----─:::::::::           |     |  :: AND ::--(z11)--::::::::
  (rmd)------------------------------|-----+--:::::::::         :: OR ::--(crn)
                                     +--------------------------::::::::

--------------------------------------------------

    (X23)----─+-─:::::::::    v
              |  :: XOR ::--(mwh)-------+----─:::::::::
    (Y23)--+--|--:::::::::              |     :: XOR ::-----------------(z23)
           |  |                         |  +--:::::::::
           |  +--:::::::::    v         |  |
           |     :: AND ::--(ggt)----+  +--|--:::::::::
           +----─:::::::::           |     |  :: AND ::--(pdp)--::::::::
  (ftg)------------------------------|-----+--:::::::::         :: OR ::--(mbg)
                                     +--------------------------::::::::

--------------------------------------------------

    (X35)----─+-─:::::::::
              |  :: XOR ::--(bhv)-------+----─:::::::::                   v
    (Y35)--+--|--:::::::::              |     :: XOR ::-----------------(hqk)
           |  |                         |  +--:::::::::
           |  +--:::::::::    v         |  |
           |     :: AND ::--(z35)----+  +--|--:::::::::
           +----─:::::::::           |     |  :: AND ::--(bvg)--::::::::
  (kwj)------------------------------|-----+--:::::::::         :: OR ::--(cfp)
                                     +--------------------------::::::::
-}

--                       A       OP       B      RES
type Network' = M.Map String [(String, String, String)]

day24b :: String -> String
day24b = part2 . parseLines'

parseLines' :: String -> Network'
parseLines' = go . splitOn [""] . lines
  where
    go [_, is] = parseInstructions' is
    go _ = error "???"

parseInstructions' :: [String] -> Network'
parseInstructions' = foldr go M.empty
  where
    go [a,b,c,_,'A','N','D',_,d,e,f,_,_,_,_,g,h,i] = addIn [a,b,c] [d,e,f] "AND" [g,h,i]
    go [a,b,c,_,'X','O','R',_,d,e,f,_,_,_,_,g,h,i] = addIn [a,b,c] [d,e,f] "XOR" [g,h,i]
    go [a,b,c,_,'O','R',_,d,e,f,_,_,_,_,g,h,i] = addIn [a,b,c] [d,e,f] "OR" [g,h,i]
    go _ = undefined

addIn :: String -> String -> String -> String -> Network' -> Network'
addIn a b op res nw = M.insertWith (++) b [(op, a, res)] nw'
  where
    nw' = M.insertWith (++) a [(op, b, res)] nw

part2 :: Network' -> String
part2 nw = show $ map (uncurry getFullAdderInvalidGates) allStarters
  where
    getFullAdderInvalidGates :: String -> String -> [String]
    getFullAdderInvalidGates x y = catMaybes [
        if not validX then Just x else Nothing,
        if not validY then Just y else Nothing,
        if isNothing mM || not validM then Just m else Nothing,
        if isNothing mN || not validN then Just n else Nothing
        -- if isNothing z || isJust mZ then Just z else Nothing
        -- traceShow (m, mZ) $ if isJust mZ then Just z else Nothing
        -- if isNothing mC || not validC then Just c else Nothing
        -- traceShow (n, c) $ if isNothing mC then Just c else Nothing
      ]
      where
        validX = validXYOps $ nw M.! x
        validY = validXYOps $ nw M.! y

        m = fromJust $ getM $ nw M.! x
        mM = M.lookup m nw
        validM = validXYOps $ fromJust mM

        n = getN $ nw M.! x
        mN = M.lookup n nw
        validN = validNOps $ nw M.! n

        z = getM $ nw M.! m
        mZ = M.lookup (fromJust z) nw

        c = getC $ nw M.! n
        mC = M.lookup c nw
        validC = validXYOps $ fromJust mC

allStarters :: [(String, String)]
allStarters = [(printf "x%02d" n, printf "y%02d" n) | n <- [1..43] :: [Int]]

validXYOps :: [(String, String, String)] -> Bool
validXYOps [(op1,_,_), (op2,_,_)] = (op1 == "AND" && op2 == "XOR") ||
                                    (op1 == "XOR" && op2 == "AND")
validXYOps _                      = False

validNOps :: [(String, String, String)] -> Bool
validNOps [(op1,_,_)] = op1 == "OR"
validNOps _           = False

getM :: [(String, String, String)] -> Maybe String
getM ((op, _, res):xs)
  | op == "XOR" = Just res
  | otherwise = getM xs
getM [] = Nothing

getN :: [(String, String, String)] -> String
getN ((op, _, res):xs)
  | op == "AND" = res
  | otherwise = getN xs
getN [] = error "impossible"

getC :: [(String, String, String)] -> String
getC ((op, _, res):xs)
  | op == "OR" = res
  | otherwise = getC xs
getC [] = error "impossible"
