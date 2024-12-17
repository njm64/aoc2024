module Day13 where
import Aoc qualified
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Bifunctor
import Util

-- Simultaneous equations:
-- x = a * xa + b * xb
-- y = a * ya + b * yb

-- Rewrite the first one in terms of b
-- b = (x - a * xa) / xb 

-- Solve for a:
-- (a * ya) + (x - a * xa) / xb * yb = y
-- (a * ya) + (x * yb / xb) - (a * xa * yb / xb) = y
-- (a * ya) - (a * xa * yb / xb) = y - (x * yb / xb)
-- a * (ya - xa * yb / xb) = y - (x * yb / xb)
-- a = (y - x * yb / xb) / (ya - xa * yb / xb)

type Equation = (Int, Int, Int) -- (x, xa, xb) where x = a * xa + b * xb

parse :: [String] -> [(Equation, Equation)]
parse = map parseEquations . splitWith null

parseEquations :: [String] -> (Equation, Equation)
parseEquations [lineA, lineB, lineP] =
  let (xa, ya) = parseButton lineA in 
  let (xb, yb) = parseButton lineB in
  let (x, y) = parsePrice lineP in
  ((x, xa, xb), (y, ya, yb))
  
parseButton :: String -> (Int, Int)
parseButton s =
  let tok = words . replace "X+" "" . replace ", Y+" " " $ s in
  (read $ tok !! 2, read $ tok !! 3)

parsePrice :: String -> (Int, Int)
parsePrice s = 
  let tok = words . replace "X=" "" . replace ", Y=" " " $ s in
  (read $ tok !! 1, read $ tok !! 2)

solve :: (Equation, Equation) -> Maybe (Int, Int)
solve (ex, ey) =
  let (x, xa, xb) = map3 toRational ex in
  let (y, ya, yb) = map3 toRational ey in
  let a = (y - x * yb / xb) / (ya - xa * yb / xb) in
  let b = (x - a * xa) / xb in
  if denominator a == 1 && denominator b == 1 
     then Just (fromIntegral $ numerator a, fromIntegral $ numerator b)
     else Nothing

tokens :: (Int, Int) -> Int
tokens (a, b) = 3 * a + b

fixUnits :: Equation -> Equation 
fixUnits (p, a, b) = (p + 10000000000000, a, b)

part1 = sum . map tokens . mapMaybe solve
part2 = part1 . map (bimap fixUnits fixUnits)
run = Aoc.run 13 parse part1 part2
