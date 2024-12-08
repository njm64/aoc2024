module Day8 where
import Aoc qualified
import Data.List
import Data.Array

type Point = (Int, Int)
type Frequency = Char
type Antenna = (Point, Frequency)
type Map = Array Point Char
type NodeFn = Point -> Point -> [Point]
type Bounds = (Point, Point)

parse :: [String] -> Map
parse lines = 
  let width = length (head lines) in
  let height = length lines in
  listArray ((1,1), (width, height)) (concat lines)

nodes :: Bounds -> Point -> Point -> [Point]
nodes b (x1,y1) (x2,y2) =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  filter (inRange b) [(x1 - dx, y1 - dy), (x2 + dx, y2 + dy)]

nodes2 :: Bounds -> Point -> Point -> [Point]
nodes2 b (x1,y1) (x2,y2) =
  let s1 = takeWhile (inRange b) (zip [x1,x2..] [y1,y2..]) in
  let s2 = takeWhile (inRange b) (zip [x2,x1..] [y2,y1..]) in
  s1 ++ s2

nodesForFrequency :: NodeFn -> [Antenna] -> Frequency -> [Point]
nodesForFrequency nodeFn antennas frequency = 
  let points = [p | (p, f) <- antennas, f == frequency] in
  nub . concat $ [nodeFn a b | a <- points, b <- points,  a /= b]

countNodes :: Map -> NodeFn -> Int
countNodes m f =
  let antennas = filter (\(p,a) -> a /= '.')  (assocs m) in
  let frequencies = nub . map snd $ antennas in
  length . nub $ concatMap (nodesForFrequency f antennas) frequencies

part1 m = countNodes m (nodes (bounds m))
part2 m = countNodes m (nodes2 (bounds m))
run = Aoc.run 8 parse part1 part2
