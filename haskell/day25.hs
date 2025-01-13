module Day25 where
import Aoc qualified
import Data.List
import Util

type HeightMap = [Int]

parseHeightMap :: [String] -> HeightMap
parseHeightMap = map (subtract 1 . length . takeWhile (== '#')) . transpose

isKey :: [String] -> Bool
isKey lines = '.' `elem` (head lines)

parse :: [String] -> ([HeightMap], [HeightMap])
parse lines = 
  let chunks = splitWith null lines in
  let (keyChunks, lockChunks) = partition isKey chunks in
  let keys = map (parseHeightMap . reverse) keyChunks in
  let locks = map parseHeightMap lockChunks in
  (keys, locks)

fits :: [Int] -> [Int] -> Bool
fits key lock = all ((< 6) . uncurry (+)) (zip key lock)

part1 (keys, locks) = length [1 | k <- keys, l <- locks, k `fits` l]
part2 _ = 0

run = Aoc.run 25 parse part1 part2
