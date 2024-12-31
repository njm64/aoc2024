module Day21 where
import Aoc qualified
import Data.List
import Data.Maybe
import Util

type Pos = (Int, Int)

numPos :: Char -> Pos
numPos c = (i `mod` 3, i `div` 3)
  where i = fromJust $ elemIndex c "789456123X0A"

arrowPos :: Char -> Pos
arrowPos c = (i `mod` 3, i `div` 3)
  where i = fromJust $ elemIndex c "X^A<v>"

neighbours :: Pos -> Pos -> String -> [(Pos, String)]
neighbours (x, y) (dstX, dstY) presses = catMaybes [xnode, ynode]
  where xnode | x < dstX = Just ((x + 1, y), presses ++ ">")
              | x > dstX = Just ((x - 1, y), presses ++ "<")
              | otherwise = Nothing
        ynode | y < dstY = Just ((x, y + 1), presses ++ "v")
              | y > dstY = Just ((x, y - 1), presses ++ "^")
              | otherwise = Nothing

costForArrow :: Int -> Pos -> Pos -> Int
costForArrow = curry3 . memoize $ uncurry3 f
  where 
    f numRobots src dst =
     go [(src, "")] maxBound
      where go [] best = best
            go ((pos, presses):ns) best
              | pos == dst =
                let r = costForArrowSeq (numRobots - 1) (presses ++ "A") in
                go ns (min best r)
              | pos == (0, 0) = go ns best
              | otherwise = go ((neighbours pos dst presses) ++ ns) best

costForArrowSeq :: Int -> String -> Int
costForArrowSeq 0 keys = length keys
costForArrowSeq numRobots keys =
  let ps = map arrowPos keys in
  sum [costForArrow numRobots src dst | (src, dst) <- zip ((2, 0):ps) ps]

costForNum :: Int -> Pos -> Pos -> Int
costForNum numRobots src dst =
  go [(src, "")] maxBound 
    where go [] best = best
          go ((pos, presses):ns) best
            | pos == dst = 
              let r = costForArrowSeq numRobots (presses ++ "A") in
              go ns (min best r) 
            | pos == (0, 3) = go ns best
            | otherwise = go ((neighbours pos dst presses) ++ ns) best

costForNumSeq :: Int -> String -> Int
costForNumSeq numRobots s =
  let ps = map numPos s in
  sum [costForNum numRobots src dst | (src, dst) <- zip ((2, 3):ps) ps]

complexity :: Int -> String -> Int
complexity numRobots code = read (init code) * costForNumSeq numRobots code

part1 = sum . map (complexity 2)
part2 = sum . map (complexity 25)
run = Aoc.run 21 id part1 part2

