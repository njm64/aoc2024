module Day4 where
import Aoc qualified
import Data.Array

type Coord = (Int, Int)
type WordMap = Array Coord Char

parse :: [String] -> WordMap
parse lines =
  let width = length (head lines) in
  let height = length lines in
  listArray ((1,1), (width, height)) (concat lines)

countAtPoint :: WordMap -> Coord -> Int
countAtPoint m (x,y) = length $ filter checkDir dirs
  where dirs = [(dx,dy) | dx <- [-1..1], dy <- [-1..1]]
        checkDir (dx,dy) = [m ! p | i <- [0..3], 
                            let p = (x + dx * i, y + dy * i), 
                            inRange (bounds m) p ] == "XMAS"

isCross :: WordMap -> Coord -> Bool
isCross m (x,y) = 
  let deltas = [(-1,-1), (1,-1), (0,0), (-1,1), (1,1)] in
  let w = [m ! (x + dx, y + dy) | (dx,dy) <- deltas] in
  elem w ["MSAMS", "SMASM", "SSAMM", "MMASS"]

part1 m = sum . map (countAtPoint m) $ indices m

part2 m = 
  let (w,h) = snd $ bounds m in
  length [1 | x <- [2..w-1], y <- [2..h-1], isCross m (x,y)]

run = Aoc.run 4 parse part1 part2

