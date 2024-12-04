module Day3 where
import Aoc qualified
import Data.Char
import Data.Maybe
import Data.List

data Cmd = Mul Int Int | Do | Dont deriving(Show)

parse :: [String] -> [Cmd]
parse = mapMaybe parseCmd . tails . unlines

-- Regex would probably be simpler here, but the goal is to avoid
-- any external packages.
parseCmd :: String -> Maybe Cmd
parseCmd s
  | "mul(" `isPrefixOf` s = parseMul (drop 4 s)
  | "do()" `isPrefixOf` s = Just Do
  | "don't()" `isPrefixOf` s = Just Dont
  | otherwise = Nothing

parseMul :: String -> Maybe Cmd
parseMul s = do
  i <- elemIndex ')' s
  let t = take i s
  j <- elemIndex ',' t
  a <- parseNum (take j t)
  b <- parseNum (drop (j + 1) t)
  return (Mul a b)

parseNum :: String -> Maybe Int
parseNum s
  | length s >= 1 && length s <= 3 && all isDigit s = Just (read s)
  | otherwise = Nothing

part1 :: [Cmd] -> Int
part1 = sum . map calc
  where calc (Mul a b) = a * b
        calc _ = 0

part2 :: [Cmd] -> Int
part2 = go 0 True
  where go total _ [] = total
        go total _ (Do : cs) = go total True cs
        go total _ (Dont : cs) = go total False cs
        go total True (Mul a b : cs) = go (total + a * b) True cs
        go total enabled (_:cs) = go total enabled cs

run = Aoc.run 3 parse part1 part2
