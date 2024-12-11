module Day11 where
import Aoc qualified
import qualified Data.Map as Map
import Data.List
import Util

parse :: [String] -> [Int]
parse = map read . words . head

evenDigits :: Int -> Bool
evenDigits = even . length . show

splitDigits :: Int -> (Int, Int)
splitDigits n =
  let s = show n in
  let (l, r) = splitAt ((length s) `div` 2) s in
  (read l, read r)

count :: Int -> Int -> Int
count n s 
  | n == 0 = 1
  | s == 0 = count (n - 1) 1
  | evenDigits s = let (left, right) = splitDigits s in
                   count (n - 1) left + count (n - 1) right
  | otherwise = count (n - 1) (s * 2024) 

part1 = sum . map (count 25)
part2 = sum . map (count 75) -- TODO: Need memoization here
run = Aoc.test 11 parse part1 part2

