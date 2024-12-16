module Day12 where
import Aoc qualified
import Data.Array
import Data.Maybe
import Data.List
import qualified Data.Array.ST as S
import Control.Monad.ST
import Control.Monad
import Util

type Pos = (Int, Int)
type Map = Array Pos Char
type Region = (Char, [Pos])
data Direction = North | South | East | West deriving (Show, Eq, Ord)
type Edge = (Direction, Int, Int)

allDirections = [North, South, East, West]

parse :: [String] -> Map
parse lines =
  let width = length (head lines) in
  let height = length lines in
  listArray ((1,1), (width, height)) (concat lines)

regions :: Map -> [Region]
regions m =
  runST $ do
    vm <- S.newArray (bounds m) False :: ST s (S.STArray s Pos Bool)
    regions <- forM (indices m) $ \p -> do
      visited <- S.readArray vm p
      if visited
         then return Nothing
         else do
           let c = m ! p
           r <- regionAtPoint m vm p c
           return (Just r)
    return (catMaybes regions)

regionAtPoint :: Map -> S.STArray s Pos Bool -> Pos -> Char -> ST s Region
regionAtPoint m vm p c =
  go [p] []
    where
      go [] acc = return (c, acc)
      go (p:ps) acc = do
        visited <- S.readArray vm p
        if visited || m ! p /= c
           then go ps acc
           else do
             S.writeArray vm p True
             let ns = filter (inRange (bounds m)) $ neighbours p
             go (ns ++ ps) (p : acc)

neighbour :: Pos -> Direction -> Pos
neighbour (x, y) North = (x, y - 1)
neighbour (x, y) South = (x, y + 1)
neighbour (x, y) East = (x - 1, y)
neighbour (x, y) West = (x + 1, y)

neighbours :: Pos -> [Pos]
neighbours p = map (neighbour p) allDirections

makeEdge :: Direction -> Pos -> Edge
makeEdge North (x, y) = (North, y, x)
makeEdge South (x, y) = (South, y, x)
makeEdge East (x, y) = (East, x, y)
makeEdge West (x, y) = (West, x, y)

edges :: Map -> Region -> [Edge]
edges m (c, ps) = catMaybes [checkEdge p d | p <- ps, d <- allDirections]
  where checkEdge p d =
          let n = neighbour p d in
          if inRange (bounds m) n && m ! n == c 
            then Nothing
            else Just (makeEdge d p)

groupEdges :: [Edge] -> [[Edge]]
groupEdges = Util.groupBy' f . sort
  where f (d1, a1, b1) (d2, a2, b2) =
          d1 == d2 && a1 == a2 && b1 == b2 - 1

part1 m = sum . map price $ regions m
  where price r@(c, ps) = length ps * length (edges m r)

part2 m = sum . map price $ regions m
  where price r@(c, ps) = length ps * length (groupEdges (edges m r))

run = Aoc.run 12 parse part1 part2

