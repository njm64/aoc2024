module Util where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Array
import System.IO.Unsafe
import Data.IORef

-- Split a list exactly once at the given delimiter
splitPair :: Eq a => a -> [a] -> ([a], [a])
splitPair c s = case break (== c) s of
  (x, []) -> (x, [])
  (x, xs) -> (x, tail xs)

-- Split a list on elements that match the given predicate
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = case break f xs of
  (ys, []) -> [ys]
  (ys, [z]) -> ys : [[]]
  (ys, _:zs) -> ys : splitWith f zs

-- Split a list on elements that are equal to c
splitList :: Eq a => a -> [a] -> [[a]]
splitList c = splitWith (== c)

-- Split a list into chunks of length n
chunks :: Int -> [a] -> [[a]]
chunks n lst = case splitAt n lst of
  (chunk, []) -> [chunk]
  (chunk, rest) -> chunk : chunks n rest

replace :: String -> String -> String -> String
replace from to = 
  let from' = Text.pack from
      to' = Text.pack to
  in Text.unpack . Text.replace from' to' . Text.pack

listToPair :: [a] -> (a, a)
listToPair [a,b] = (a, b)

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (a, b, c)  = (f a, f b, f c)

 -- Find the first index of a substring
position :: String -> String -> Int
position s substr =
  step s 0
  where step [] acc = -1
        step t acc
          | substr `List.isPrefixOf` t = acc
          | otherwise = step (tail t) (acc + 1)

-- Like Data.List.groupBy, but compare with the nearest
-- neighbour, instead of the last element in the group.
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f [] = []
groupBy' f (x:xs) = step xs [[x]]
  where step [] acc = reverse . map reverse $ acc
        step (x:xs) (a:as) =
          if f (head a) x then step xs ((x:a):as)
          else step xs ([x]:a:as)

groupOn :: Eq a => (t -> a) -> [t] -> [[t]]
groupOn f = groupBy' (\a b -> f a == f b)
  
arrayRows :: Array (Int, Int) a -> [[a]]
arrayRows a =
  [row y | y <- [y1..y2]]
  where row y = [a ! (x, y) | x <- [x1..x2]]
        ((x1, y1), (x2, y2)) = bounds a

showCharArray :: Array (Int, Int) Char -> String
showCharArray = unlines . arrayRows

-- Apply a function to the nth element of a list
updateNth :: (a -> a) -> Int -> [a] -> [a]
updateNth f _ [] = []
updateNth f 0 (x:xs) = f x : updateNth f (-1) xs
updateNth f i (x:xs) = x : updateNth f (i-1) xs
  
-- Apply a function N times
applyN :: Int -> (a -> a) -> a -> a
applyN n f a = (iterate f a) !! n

-- Create an array with a default value in every cell
arrayWithDefault :: Ix i => (i, i) -> a -> Array i a
arrayWithDefault b def = array b [(i, def) | i <- range b]

-- From https://stackoverflow.com/questions/141650/how-do-you-make-a-generic-memoize-function-in-haskell
memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do 
    r <- newIORef Map.empty
    return $ \x -> unsafePerformIO $ do 
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do 
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y

