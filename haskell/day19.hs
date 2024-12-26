module Day19 where
import Aoc qualified
import Data.List
import Util

type Towel = String
type Design = String

parse :: [String] -> ([Towel], [Design])
parse lines =
  let towels = words . replace "," " " $ head lines in
  let designs = drop 2 lines in
  (towels, designs)

countDesign :: [Towel] -> Design -> Int
countDesign ts = memoize f
  where
    f [] = 1
    f d = sum . map count $ ts
      where count t = case stripPrefix t d of
                        Just suffix -> countDesign ts suffix
                        Nothing -> 0

part1 (towels, designs) = length [d | d <- designs, countDesign towels d > 0]
part2 (towels, designs) = sum $ map (countDesign towels) designs
run = Aoc.run 19 parse part1 part2

