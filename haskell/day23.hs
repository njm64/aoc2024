module Day23 where
import Aoc qualified
import Data.List
import Data.Tuple
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Util

type Links = [String]
type Graph = Map String Links

parse :: [String] -> Graph
parse lines = buildMap (links ++ reverseLinks)
  where links = map (splitPair '-') lines
        reverseLinks = map swap links
        buildMap = foldl' (\m (k, v) -> Map.insertWith (++) k [v] m) Map.empty 

findLinks :: Graph -> String -> Links
findLinks g k = Map.findWithDefault [] k g

bronker3 :: Graph -> Links -> Links -> Links -> [Links]
bronker3 g r p x
  | length r == 3 = [r]
  | otherwise = go p x []
  where 
    go [] _ acc = acc
    go (v:p) x acc =
      let sets = bronker3 g
                   (r `union` [v])
                   (p `intersect` (findLinks g v))
                   (x `intersect` (findLinks g v)) in
      go p (x `union` [v]) (sets ++ acc)

bronkerbosch :: Graph -> Links -> Links -> Links -> [Links]
bronkerbosch _ r [] [] = [r]
bronkerbosch g r p x = go p x []
  where 
    go [] _ acc = acc
    go (v:p) x acc =
      let sets = bronkerbosch g
                   (r `union` [v])
                   (p `intersect` (findLinks g v))
                   (x `intersect` (findLinks g v)) in
      go p (x `union` [v]) (sets ++ acc)

part1 g = let sets = bronker3 g [] (Map.keys g) [] in
  show . length . filter (any (\s -> head s == 't')) $ sets

part2 g = let sets = bronkerbosch g [] (Map.keys g) [] in
  intercalate "," $ maximumBy (compare `on` length) sets

run = Aoc.run 23 parse part1 part2
