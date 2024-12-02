module Day1 where
import Aoc qualified
import Data.List
import Util

parse :: [String] -> [[Int]]
parse = transpose . map (map read . words)

part1 [list1, list2] =
  sum [abs (a - b) | (a, b) <- zip (sort list1) (sort list2)]

part2 [list1, list2] =
  sum [i | i <- list1, j <- list2, i == j]

run = Aoc.run 1 parse part1 part2
