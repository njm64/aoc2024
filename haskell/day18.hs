module Day18 where
import Aoc qualified
import Data.List
import Data.Array
import Data.Maybe
import qualified Data.Map as Map
import Util
import Heap

type Pos = (Int, Int)
type Map = Array Pos Char

parse :: [String] -> [Pos]
parse = map parsePos

parsePos :: String -> Pos
parsePos s = 
  let (lhs, rhs) = splitPair ',' s in
  (read lhs, read rhs)

buildMap :: Int -> [Pos] -> Map
buildMap n ps =
  let a = listArray ((0, 0), (n, n)) (repeat '.') in
  a // [(p, '#') | p <- ps]

neighbours :: Pos -> [Pos]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

solve :: Map -> Pos -> Pos -> Maybe Int
solve m start goal =
  go (Heap.push Heap.empty 0 start) Map.empty where
    go heap costMap =
      case Heap.top heap of
        Nothing -> Nothing
        Just (cost, pos) ->
          if pos == goal then Just cost
             else let items = [(c, p) | p <- neighbours pos,
                                        inRange (bounds m) p,
                                        m ! p == '.',
                                        let c = cost + 1,
                                        checkCost costMap c p]
                      heap' = foldl' updateHeap (Heap.pop heap) items
                      costMap' = foldl' updateMap costMap items
                    in go heap' costMap'
    updateHeap h (cost, pos) = Heap.push h cost pos
    updateMap m (cost, pos) = Map.insert pos cost m
    checkCost m cost pos = case Map.lookup pos m of
                             Just existing -> cost < existing
                             Nothing -> True

part1 ps =
  let m = buildMap 70 (take 1024 ps) in
  let (start, end) = (bounds m) in
  fromJust $ solve m start end

part2 = part1

run = Aoc.run 18 parse part1 part2

