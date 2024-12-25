module Day17 where
import Aoc qualified
import Data.List
import Data.Array
import Data.Bits
import Data.Maybe
import Util
import Debug.Trace

type Program = [Int]

data CPU = CPU {
 cpuA :: Int,
 cpuB :: Int,
 cpuC :: Int,
 cpuIP :: Int,
 cpuOut :: [Int]
} deriving (Show)

parse :: [String] -> (CPU, Program)
parse lines = 
  (CPU {cpuA = a, cpuB = b, cpuC = c, cpuIP = 0, cpuOut = []}, p) 
    where a = parseRegister (head lines)
          b = parseRegister (lines !! 1)
          c = parseRegister (lines !! 2)
          p = parseProgram (lines !! 4)

parseRegister :: String -> Int
parseRegister = read . last . words

parseProgram :: String -> [Int]
parseProgram = map read . splitList ',' . last . words

combo :: CPU -> Int -> Int
combo _ n
  | n >= 0 && n <= 3 = n
combo cpu 4 = cpuA cpu
combo cpu 5 = cpuB cpu
combo cpu 6 = cpuC cpu
combo cpu n = error (" operand " ++ show n)

op :: CPU -> Int -> Int -> CPU
op c 0 n = c {cpuA = cpuA c `shiftR` (combo c n)}
op c 1 n = c {cpuB = cpuB c `xor` n}
op c 2 n = c {cpuB = combo c n `mod` 8}
op c 3 n = if cpuA c == 0 then c else c { cpuIP = n }
op c 4 n = c {cpuB = cpuB c `xor` cpuC c}
op c 5 n = c {cpuOut = combo c n `mod` 8 : cpuOut c}
op c 6 n = c {cpuB = cpuA c `shiftR` combo c n}
op c 7 n = c {cpuC = cpuA c `shiftR` combo c n}

exec :: CPU -> Program -> [Int]
exec cpu program =
  let ip = cpuIP cpu in
  if ip >= length program
     then reverse (cpuOut cpu)
     else let opcode = program !! ip in
          let operand = program !! (ip + 1) in
          let cpu' = cpu {cpuIP = ip + 2} in
          let cpu'' = op cpu' opcode operand in
          exec cpu'' program

-- Run one iteration of the program, and return the digit printed.
-- Our input disassembles to this:
--
-- 2,4  BST A     b = a % 8;     
-- 1,2  BXL 2     b = b ^ 2;
-- 7,5  CDV B     c = a >> b;
-- 4,1  BXC       b = b ^ c
-- 1,3  BXL 3     b = b ^ 3;
-- 5,5  OUT B     print b;
-- 0,3  ADV 3     a = a >> 3;
-- 3,0  JNZ 0
--
-- This function is effectively just a translation to Haskell
-- of the first 6 instructions above.
runIteration :: Int -> Int
runIteration a =
  let b = a `mod` 8 `xor` 2 in
  let c = (a `shiftR` b) `mod` 8 in
  b `xor` c `xor` 3

-- Given a list of digits in reverse order, and value of A that
-- matches digits already processed, find the lowest value of A
-- that will match the given digits.
findSolution :: [Int] -> Int -> Maybe Int
findSolution [] a = Just a
findSolution (d:ds) a =
  let base = a * 8 in
  let possibles = [base .. base + 7] in
  let as = [a' | a' <- possibles, runIteration a' == d] in
  listToMaybe $ mapMaybe (findSolution ds) as

part1 (cpu, mem) = show $ exec cpu mem
part2 (cpu, mem) = show . fromJust $ findSolution (reverse mem) 0

run = Aoc.run 17 parse part1 part2

