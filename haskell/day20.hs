module Day20 where
import Aoc qualified
import Data.List
import Data.Array
import qualified Heap
import qualified Data.Map as Map

type Pos = (Int, Int)
type Cheat = (Pos, Pos)
type Map = Array Pos Char
type TimeMap = Map.Map Pos Int

parse :: [String] -> Map
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  array ((1,1), (width, height)) (zip coords (concat lines))

findPos :: Map -> Char -> Pos
findPos m c = head [i | i <- indices m, m ! i == c]

neighbours :: Pos -> [Pos]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isPassable :: Char -> Bool
isPassable c = c == '.' || c == 'S' || c == 'E'

getTime :: TimeMap -> Pos -> Int
getTime m p = Map.findWithDefault maxBound p m

-- Build a map of the times to travel from the start position to all the
-- other positions in the graph.
buildTimeMap :: Map -> Pos -> TimeMap
buildTimeMap m start =
  go initialHeap initialMap where
    initialHeap = Heap.push Heap.empty 0 start
    initialMap = Map.fromList [(start, 0)]
    go heap timeMap =
      case Heap.top heap of
        Nothing -> timeMap
        Just (time, pos) ->
          let items = [(t, p) | p <- neighbours pos,
                                     inRange (bounds m) p,
                                     isPassable (m ! p),
                                     let t = time + 1,
                                     t < getTime timeMap p]
              heap' = foldl' updateHeap (Heap.pop heap) items
              timeMap' = foldl' updateMap timeMap items
          in go heap' timeMap'
    updateHeap h (cost, pos) = Heap.push h cost pos
    updateMap m (cost, pos) = Map.insert pos cost m

manhattanDistance :: Pos -> Pos -> Int
manhattanDistance (x1, y1) (x2, y2) = abs(x1 - x2) + abs (y1 - y2)

findCheats :: Map -> Int -> [(Pos, Pos)]
findCheats m maxCheat =
  [((x1, y1), (x2, y2)) | (x1, y1) <- indices m,
                          isPassable (m ! (x1, y1)),
                          x2 <- [x1 - maxCheat .. x1 + maxCheat],
                          y2 <- [y1 - maxCheat .. y1 + maxCheat],
                          inRange (bounds m) (x2, y2),
                          isPassable (m ! (x2, y2)),
                          manhattanDistance (x1, y1) (x2, y2) <= maxCheat]

-- First, build a map of times from the start position to all other positions,
-- and a map of times from the end position to all other positions. Then to
-- calculate the time for a cheat, we just take the manhattan distance of
-- the cheat, and add that to the time from the mapStart to the cheatStart,
-- plus the time from cheatEnd to mapEnd.
calc :: Map -> Int -> Int
calc m maxCheat =
  length $ filter (\c -> cheatTime c <= baseTime - 100) cheats
    where
      start = findPos m 'S'
      end = findPos m 'E'
      startMap = buildTimeMap m start
      endMap = buildTimeMap m end
      baseTime = getTime startMap end
      cheats = findCheats m maxCheat
      cheatTime (start, end) = getTime startMap start + 
                               getTime endMap end + 
                               manhattanDistance start end

part1 m = calc m 2
part2 m = calc m 20
run = Aoc.run 20 parse part1 part2
