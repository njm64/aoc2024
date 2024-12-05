module Day5 where
import Aoc qualified
import Data.List
import Util

type Rule = (Int, Int)
type Update = [Int]

parse :: [String] -> ([Rule], [Update])
parse lines =
  let [rules, updates] = splitWith null lines in
  (map parseRule rules, map parseUpdate updates)

parseRule :: String -> Rule
parseRule = listToPair . map read . splitList '|' 

parseUpdate :: String -> Update
parseUpdate = map read . splitList ','

check :: Update -> Rule -> Bool
check (u:us) (a, b)
  | u == b = a `notElem` us
  | otherwise = check us (a, b)
check _ _ = True

checkAll :: [Rule] -> Update -> Bool
checkAll rs u = all (check u) rs

middle :: [a] -> a
middle u = u !! (length u `div` 2)

applyRule :: Rule -> Update -> Update
applyRule (a,b) u =
  let u' = delete a u in
  case elemIndex b u' of
    Just i -> (take i u') ++ [a] ++ (drop i u')
    Nothing -> u

fix :: [Rule] -> Update -> Update
fix rs u =
  case find (not . check u) rs of
    Just r -> fix rs (applyRule r u)
    Nothing -> u

part1 (rules, updates) =
  let validUpdates = filter (checkAll rules) updates in
  sum $ (map middle) validUpdates 

part2 (rules, updates) = 
  let invalidUpdates = filter (not . checkAll rules) updates in
  let fixedUpdates = map (fix rules) invalidUpdates in
  sum $ (map middle) fixedUpdates

run = Aoc.run 5 parse part1 part2

