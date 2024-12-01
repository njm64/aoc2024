module Day1 where
import Util
import Data.List
import qualified Aoc

parse :: [String] -> [[Int]]
parse = transpose . map (map read . words)
    
part1 [list1, list2] =
    sum . map distance $ zip (sort list1) (sort list2)
        where distance (a, b) = abs (a - b)

part2 [list1, list2] = 
    sum [i | i <- list1, j <- list2, i == j]

run = Aoc.run 1 parse part1 part2

