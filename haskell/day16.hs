module Day16 where
import Aoc qualified
import Data.List
import Data.Array
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Heap
import Util

type Pos = (Int, Int)
type Map = Array Pos Char
data Dir = North | East | South | West deriving (Show, Enum, Eq, Ord)
type State = (Pos, Dir)
type Cost = Int
type CostMap = Map.Map State (Int, [State])

allDirs :: [Dir]
allDirs = [North, East, South, West]

parse :: [String] -> Map
parse lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  array ((1,1), (width, height)) (zip coords (concat lines))

findPos :: Map -> Char -> Pos
findPos m c = head [p | (p, c') <- assocs m, c == c']

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

getCost :: CostMap -> State -> Cost
getCost m state =
  case Map.lookup state m of
    Just (existingCost, _) -> existingCost
    Nothing -> maxBound

getParents :: CostMap -> State -> [State]
getParents m state = 
  case Map.lookup state m of
    Just (_, existingParents) -> existingParents
    Nothing -> []

updateMap :: State -> CostMap -> (Cost, State) -> CostMap
updateMap parentState m (cost, state) =
  let existingCost = getCost m state in
  let existingParents = getParents m state in
  if cost > existingCost 
     then m
     else if cost == existingCost
     then Map.insert state (cost, nub (parentState:existingParents)) m
     else Map.insert state (cost, [parentState]) m
  
buildCostMap :: Map -> State -> CostMap
buildCostMap m initialState =
  go (Heap.push Heap.empty 0 initialState) Map.empty where
    go heap costMap =
      case Heap.top heap of
        Nothing -> costMap
        Just (cost, state) ->
         let items = [(c,s) | (c,s) <- neighbours (cost, state),
                              isValidMove s, c <= getCost costMap s]
             heap' = foldl' updateHeap (Heap.pop heap) items
             costMap' = foldl' (updateMap state) costMap items
           in go heap' costMap'
    updateHeap h (cost, state) = Heap.push h cost state
    isValidMove (pos, _) = m ! pos /= '#'
                                        
findAllPositions :: CostMap -> [State] -> [Pos]
findAllPositions cm endStates =
  go endStates Set.empty where
  go [] set = nub . map fst $ Set.toList set
  go (s:ss) set =
    if Set.member s set
       then go ss set
       else let parents = snd . fromJust $ Map.lookup s cm in 
              go (parents ++ ss) (Set.insert s set)

bestEndStates ::  CostMap -> Pos -> [State]
bestEndStates cm end =
  [state | (state, cost) <- statesWithCosts, cost == minCost]
    where
      endStates = [(end, d) | d <- allDirs, (end, d) `Map.member` cm]
      statesWithCosts = [(s, costForState s) | s <- endStates]
      costForState s = fst . fromJust $ Map.lookup s cm
      minCost = minimum $ map snd statesWithCosts

part1 m = 
  let start = (findPos m 'S') in
  let end = (findPos m 'E') in
  let cm = buildCostMap m (start, East) in
  minimum . map fst $ catMaybes [(Map.lookup (end, d) cm) | d <- allDirs]

part2 m = 
  let start = (findPos m 'S') in
  let end = (findPos m 'E') in
  let cm = buildCostMap m (start, East) in
  length . findAllPositions cm $ bestEndStates cm end

run = Aoc.run 16 parse part1 part2
