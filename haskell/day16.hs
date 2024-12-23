module Day16 where
import Aoc qualified
import Data.List
import Data.Array
import qualified Data.Map as Map
import qualified Heap
import Util

type Pos = (Int, Int)
type Map = Array Pos Char
data Dir = North | East | South | West deriving (Show, Enum, Eq, Ord)
type State = (Pos, Dir)
type Cost = Int
type CostMap = Map.Map State Int

parse :: [String] -> Map
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  array ((1,1), (width, height)) (zip coords (concat lines))

findStartPos :: Map -> Pos
findStartPos m = head [p | (p, c) <- assocs m, c == 'S']

turnLeft :: Dir -> Dir
turnLeft North = West
turnLeft d = pred d

turnRight :: Dir -> Dir
turnRight West = North
turnRight d = succ d

moveForward :: Pos -> Dir -> Pos
moveForward (x, y) North = (x, y - 1)
moveForward (x, y) East = (x + 1, y)
moveForward (x, y) South = (x, y + 1)
moveForward (x, y) West = (x - 1, y)

neighbours :: (Cost, State) -> [(Cost, State)]
neighbours (c, (p, d)) = 
  [(c + 1, (moveForward p d, d)), 
   (c + 1000, (p, turnLeft d)),
   (c + 1000, (p, turnRight d))]

search :: Map -> State -> Cost
search m initialState =
  go (Heap.push Heap.empty 0 initialState) Map.empty where
    go heap costMap =
      case Heap.top heap of
        Nothing -> error "Not found"
        Just (cost, state) ->
          if isGoal state 
             then cost
             else let items = [i | i <- neighbours (cost, state),
                                   isValidMove (snd i),
                                   checkCost costMap i]
                      heap' = foldl' updateHeap (Heap.pop heap) items
                      costMap' = foldl' updateMap costMap items
                  in go heap' costMap'
    updateHeap h (cost, state) = Heap.push h cost state
    updateMap m (cost, state) = Map.insert state cost m
    isGoal (pos, _) = m ! pos == 'E'
    isValidMove (pos, _) = m ! pos /= '#'
    checkCost m (cost, state) = case Map.lookup state m of
                                  Just existing -> cost < existing
                                  Nothing -> True
                                        

part1 m = search m (findStartPos m, East)
part2 = part1

run = Aoc.run 16 parse part1 part2
