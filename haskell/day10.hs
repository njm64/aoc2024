module Day10 where
import Aoc qualified
import Data.Array
import Data.Char
import Data.List
import Util

type Position = (Int, Int)
type Map = Array Position Int

parse :: [String] -> Map
parse lines = 
  let width = length (head lines) in
  let height = length lines in
  let values = map digitToInt $ concat lines in
  listArray ((1,1), (width, height)) values

startPositions :: Map -> [Position]
startPositions m = [i | (i, v) <- assocs m, v == 0]

neighbours :: Map -> Position -> [Position]
neighbours m (x, y) =
  let h = m ! (x, y) in
  let ns = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] in
  filter (\p -> inRange (bounds m) p && m ! p == h + 1) ns

summits :: Map -> Position -> [Position]
summits m p
  | m ! p == 9 = [p]
  | otherwise = concatMap (summits m) (neighbours m p)

score :: Map -> Position -> Int
score m p = length . nub $ summits m p 

rating :: Map -> Position -> Int
rating m p = length $ summits m p

part1 m = sum . map (score m) $ startPositions m
part2 m = sum . map (rating m) $ startPositions m
run = Aoc.run 10 parse part1 part2
