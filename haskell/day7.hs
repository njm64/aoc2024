module Day7 where
import Aoc qualified
import Util

type Rule = (Int, [Int])

parse :: [String] -> [Rule]
parse = map parseRule

parseRule :: String -> Rule
parseRule line =
  let (lhs, rhs) = splitPair ':' line in
  (read lhs, map read (words rhs))

(|+|) :: Int -> Int -> Int
(|+|) a b = read (show a ++ show b)

checkRule :: Rule -> Bool
checkRule (r, v:vs) = go v vs
    where go acc [v] = r == acc * v || r == acc + v
          go acc (v:vs) = go (acc * v) vs || go (acc + v) vs

checkRule2 :: Rule -> Bool
checkRule2 (r, v:vs) = go v vs
    where go acc [v] = r == acc * v || r == acc + v || r == acc |+| v
          go acc (v:vs) = go (acc * v) vs || go (acc + v) vs || go (acc |+| v) vs

part1 = sum . map fst . filter checkRule 
part2 = sum . map fst . filter checkRule2
run = Aoc.run 7 parse part1 part2
