module Day2 where
import Aoc qualified

parse :: [String] -> [[Int]]
parse = map (map read . words)

inRange :: Int -> Bool
inRange n = n >= 1 && n <= 3

safe :: [Int] -> Bool
safe ns = all inRange deltas || all (inRange . negate) deltas
  where deltas = [a - b | (a, b) <- zip ns (tail ns)]

dampenedSafe :: [Int] -> Bool
dampenedSafe xs = any safe xss
  where xss = xs : [removeNth i xs | i <- [0..length xs-1]] 

removeNth :: Int -> [a] -> [a]
removeNth n xs = (take n xs) ++ (drop (n + 1) xs)

part1 = length . filter safe
part2 = length . filter dampenedSafe
run = Aoc.run 2 parse part1 part2
