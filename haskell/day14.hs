module Day14 where
import Aoc qualified
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Util

type Vec2 = (Int, Int)
type Robot = (Vec2, Vec2)

mapWidth = 101
mapHeight = 103

parse :: [String] -> [(Vec2, Vec2)]
parse = map parseLine

parseLine :: String -> (Vec2, Vec2)
parseLine s = 
  let (lhs, rhs) = splitPair ' ' s in
  (parseVec lhs, parseVec rhs)

parseVec :: String -> Vec2
parseVec s = 
  let (x, y) = splitPair ',' (drop 2 s) in
  (read x, read y)

move :: Robot -> Robot
move ((x, y), (dx, dy))  =
  (((x + dx) `mod` mapWidth, (y + dy) `mod` mapHeight), (dx, dy))

isNW (x,y) = x < mapWidth `div` 2 && y < mapHeight `div` 2
isNE (x,y) = x > mapWidth `div` 2 && y < mapHeight `div` 2
isSW (x,y) = x < mapWidth `div` 2 && y > mapHeight `div` 2
isSE (x,y) = x > mapWidth `div` 2 && y > mapHeight `div` 2

part1 robots =
  let points = map fst $ applyN 100 (map move) robots in
  let nw = length $ filter isNW points in
  let ne = length $ filter isNE points in
  let sw = length $ filter isSW points in
  let se = length $ filter isSE points in
  nw * ne * sw * se

-- Check if a list of robots has at least 100 robots with a
-- matching position on the opposite side of the Y axis
checkTree :: [Robot] -> Bool
checkTree robots =
  let points = map fst robots in
  let mid = mapWidth `div` 2 in
  let left = [(x, y) | (x, y) <- points, x < mid] in
  let right = [(mapWidth - 1 - x, y) | (x, y) <- points, x > mid] in
  let lset = Set.fromList left in
  let rset = Set.fromList right in
  length (lset `Set.intersection` rset) > 100

part2 robots = 
  fromJust . elemIndex True . map checkTree $ (iterate (map move) robots)

run = Aoc.run 14 parse part1 part2


