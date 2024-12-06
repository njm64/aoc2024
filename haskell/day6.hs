module Day6 where
import Aoc qualified
import Data.Array
import Data.List
import Util

data Direction = North | East | South | West deriving (Eq, Enum, Show)
type Position = (Int, Int)
type Map = Array Position Char

parse :: [String] -> Map
parse lines = 
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  array ((1,1), (width, height)) (zip coords (concat lines))

startPosition :: Map -> Position
startPosition m = 
  head [i | (i, c) <- assocs m, c == '^']

isValidPosition :: Map -> Position -> Bool
isValidPosition m p = (inRange (bounds m) p ) && (m ! p) /= '#'

getVec North = (0, -1)
getVec South = (0, 1)
getVec West = (-1, 0)
getVec East = (1, 0)

nextDir :: Direction -> Direction
nextDir West = North
nextDir d = succ d

nextSegment :: Map -> Position -> Direction -> [Position]
nextSegment m (x,y) d =
  takeWhile (isValidPosition m) (zip [x,x+dx..] [y,y+dy..])
    where (dx,dy) = getVec d

isEdgePosition :: Map -> Position -> Bool
isEdgePosition m (x,y) =
  let ((x1,y1),(x2,y2)) = bounds m in
  x == x1 || x == x2 || y == y1 || y == y2

checkCycle :: Map -> Bool
checkCycle m = go (startPosition m) North []
  where
    go p d states
      | isEdgePosition m p = False
      | otherwise =
        let p' = last $ nextSegment m p d in
        let d' = nextDir d in
        let s = (p',d') in
        (s `elem` states) || go p' d' (s:states)

part1 :: Map -> Int
part1 m = go (startPosition m) North []
  where 
    go p d segs
      | isEdgePosition m p = length . nub $ concat segs
      | otherwise =
        let s = nextSegment m p d in
        go (last s) (nextDir d) (s:segs) 

part2 :: Map -> Int
part2 m = 
  let start = startPosition m in
  let maps = [m // [(p, '#')] | p <- indices m, p /= start] in
  length $ filter checkCycle maps

run = Aoc.run 6 parse part1 part2

