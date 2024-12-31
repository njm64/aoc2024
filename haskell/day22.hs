module Day22 where
import Aoc qualified
import Data.List
import Data.Bits
import Data.Array
import Data.Maybe
import Util

type Key = Int
type PriceIndex = Array Key Int 

parse :: [String] -> [Int]
parse = map read

prune :: Int -> Int
prune n = n `mod` 16777216

nextSecret :: Int -> Int
nextSecret s =
  let s' = prune (s `xor` (s * 64)) in
  let s'' = prune (s' `xor` (s' `div` 32)) in
  prune (s'' `xor` (s'' * 2048))

-- Build an integer key for a 4 digit sequence. We could just use
-- multi-dimensional arrays, but this works out twice as fast.
makeKey :: Int -> Int -> Int -> Int -> Int
makeKey a b c d =
  ((a + 10) * 8000 + (b + 10) * 400 + (c + 10) * 20 + (d + 10))

listToKey :: [Int] -> Maybe Key
listToKey (a:b:c:d:_) = Just (makeKey a b c d)
listToKey _ = Nothing

allKeys :: [Key]
allKeys = [makeKey a b c d | a <- r, b <- r, c <- r, d <- r]
  where r = [-9..9]

blankIndex :: PriceIndex
blankIndex = array (0, maxKey) [(k, 0) | k <- [0..maxKey]]
  where maxKey = 20*20*20*20

-- Given a seed, build an index from key to price. Pairs are applied
-- to the index in reverse order so that the we index the first match
-- rather than the last one (we're relying on GHC behaviour here).
buildIndex :: Int -> PriceIndex
buildIndex seed = 
  let digits = take 2000 . map (`mod` 10) $ iterate nextSecret seed in
  let deltas = [b - a | (a, b) <- zip digits (tail digits)] in
  let keys = mapMaybe listToKey $ tails deltas in
  let pairs = zip keys (drop 4 digits) in
  blankIndex // (reverse pairs)

part1 = sum . map (applyN 2000 nextSecret)

part2 seeds = maximum $ map totalPrice allKeys
    where indices = map buildIndex seeds
          totalPrice k = sum [i ! k | i <- indices]

run = Aoc.run 22 parse part1 part2
