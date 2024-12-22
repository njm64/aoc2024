module Day15 where
import Aoc qualified
import Data.List
import Data.Array
import Debug.Trace
import Util

data Dir = North | South | East | West deriving (Show, Eq)
type Pos = (Int, Int)
type Map = Array Pos Char

parse :: [String] -> (Map, Pos, [Dir])
parse lines =
  let (top, bottom) = splitPair "" lines in
  let m = parseMap top in
  let startPos = findStartPos m in
  let m' = m // [(startPos, '.')] in
  (m', startPos, map parseDir $ concat bottom)

parseMap :: [String] -> Map
parseMap lines =
  let width = length (head lines) in
  let height = length lines in
  let coords = [(x,y) | y <- [1..height], x <- [1..width]] in
  array ((1,1), (width, height)) (zip coords (concat lines))

parseDir :: Char -> Dir
parseDir c = case c of
  '^' -> North
  'v' -> South
  '<' -> West
  '>' -> East

findStartPos :: Map -> Pos
findStartPos m = head [i | i <- indices m, m ! i == '@']

expandMap :: Map -> Map
expandMap m =
  let (w, h) = snd (bounds m) in
  let coords = [(x,y) | y <- [1..h], x <- [1..w]] in
  let coords' = [(x,y) | y <- [1..h], x <- [1..w * 2]] in
  let tiles = concatMap (expandTile . (m !)) coords in
  array ((1, 1), (w * 2, h)) (zip coords' tiles)

expandTile :: Char -> String
expandTile '#' = "##"
expandTile 'O' = "[]"
expandTile '.' = ".."
expandTile '@' = "@."

step :: Pos -> Dir -> Pos
step (x, y) North = (x, y - 1)
step (x, y) South = (x, y + 1)
step (x, y) West = (x - 1, y)
step (x, y) East = (x + 1, y)

getTile :: Map -> Pos -> Char
getTile m p = if inRange (bounds m) p then (m ! p) else '#'

findBoxes :: Map -> Pos -> Dir -> Maybe [Pos]
findBoxes m p d =
  go [step p d] []
    where
      go [] boxes = Just boxes
      go (p:ps) boxes =
        case getTile m p of
          '.' -> go ps boxes
          'O' -> go ((step p d):ps) (p:boxes)
          '[' -> if d == East || d == West
                   then go ((step p d):ps) (p:boxes)
                   else let q = step p East in
                        go ((step p d):(step q d):ps) (p:q:boxes)
          ']' -> if d == East || d == West
                   then go ((step p d):ps) (p:boxes)
                   else let q = step p West in
                            go ((step p d):(step q d):ps) (p:q:boxes)
          '#' -> Nothing

moveBoxes :: Map -> [Pos] -> Dir -> Map
moveBoxes m ps d =
  m // [(p, '.') | p <- ps] // [(step p d, (m ! p)) | p <- ps]

move :: (Map, Pos) -> Dir -> (Map, Pos)
move (m, p) d =
  case findBoxes m p d of
    Just boxes -> (moveBoxes m boxes d, step p d)
    Nothing -> (m, p)

gps :: Pos -> Int
gps (x, y) = (y - 1) * 100 + x - 1

printMap :: Map -> String
printMap m = unlines [makeLine y | y <- [1..h]]
  where (w, h) = snd (bounds m)
        makeLine y = [m ! (x, y) | x <- [1..w]]

part1 (m, pos, dirs) =
  let (m', _) = foldl' move (m, pos) dirs in
  sum [gps p | (p, c) <- assocs m', c == 'O']

part2 (m, (x, y), dirs) = 
  let (m', _) = foldl' move ((expandMap m), (x * 2 - 1, y)) dirs in
  sum [gps p | (p, c) <- assocs m', c == '[']

-- test n = do
--   i <- Aoc.readTestInput 15
--   let (m, (x, y), dirs) = parse i
--   let (m', pos') = foldl' move2 ((expandMap m), (x*2-1, y)) (take n dirs) 
--   let mp = m' // [(pos', '@')]
--   putStrLn $ printMap mp

run = Aoc.run 15 parse part1 part2

