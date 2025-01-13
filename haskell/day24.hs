module Day24 where
import Aoc qualified
import Data.List
import Data.Bits
import Data.Maybe
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Util

data Wire = Value Int |
            AndGate String String | 
            OrGate String String | 
            XorGate String String deriving (Show, Eq)

type WireMap = Map String Wire

parse :: [String] -> WireMap
parse = Map.fromList . mapMaybe (wire . words)
  where wire [s, "1"] = Just (init s, Value 1)
        wire [s, "0"] = Just (init s, Value 0)
        wire [a, "AND", b, "->", c] = Just (c, AndGate a b)
        wire [a, "OR", b, "->", c] = Just (c, OrGate a b)
        wire [a, "XOR", b, "->", c] = Just (c, XorGate a b)
        wire _ = Nothing

resolve :: String -> State WireMap Int
resolve k = do
  wm <- get
  case Map.lookup k wm of
    Just (Value v) -> return v
    Just (AndGate a b) -> resolveGate a b k (.&.)
    Just (OrGate a b) -> resolveGate a b k (.|.)
    Just (XorGate a b) -> resolveGate a b k xor
    Nothing -> error "Wire not found"

resolveGate :: String -> String -> String -> (Int -> Int -> Int) -> State WireMap Int
resolveGate a b k op = do
  va <- resolve a
  vb <- resolve b
  let v = va `op` vb
  modify (Map.insert k (Value v))
  return v

wireKeys :: WireMap -> String -> [String]
wireKeys wm prefix = filter (prefix `isPrefixOf`) $ Map.keys wm

load :: [String] -> WireMap -> Int
load keys wm = 
  let bits = evalState (mapM resolve keys) wm in
  foldl' (\r (i, b) -> r .|. (b `shift` i)) 0 (zip [0..] bits)

store :: [String] -> Int -> WireMap -> WireMap
store keys n wm =
  let updates = [(k, Value ((n `shiftR` i) .&. 1)) | (i, k) <- zip [0..] keys] in
  Map.union (Map.fromList updates) wm

-- Given a list of keys, return a set of all dependencies
deps :: WireMap -> [String] -> Set String
deps wm keys =
  go keys Set.empty where
    go [] s = s
    go (k:ks) s =
      let s' = Set.insert k s in
      case Map.lookup k wm of
        Just (AndGate a b) -> go (a:b:ks) s'
        Just (OrGate a b) -> go (a:b:ks) s'
        Just (XorGate a b) -> go (a:b:ks) s'
        _ -> go ks s'

checkRule :: WireMap -> Int -> (Int, Int, Int, Int) -> Bool
checkRule _ 0 (_, _, 1, _) = True -- Can't test carry if n is zero
checkRule wm n (a, b, cin, s) =
  let x = (a `shift` n) .|. (cin `shift` (n - 1)) in
  let y = (b `shift` n) .|. (cin `shift` (n - 1)) in
  let z = (s `shift` n) in
  let xkeys = wireKeys wm "x" in
  let ykeys = wireKeys wm "y" in
  let zkeys = take (n + 1) (wireKeys wm "z") in
  z == (load zkeys . store xkeys x . store ykeys y) wm

-- Check the WireMap is correct for the nth bit of z
-- Each rule is a row in the truth table (A, B, Cin, Sum)
checkBit :: WireMap -> Int -> Bool
checkBit wm n = all (checkRule wm n) rules
  where rules = [(0, 0, 0, 0),
                 (0, 0, 1, 1),
                 (0, 1, 0, 1),
                 (0, 1, 1, 0),
                 (1, 0, 0, 1),
                 (1, 0, 1, 0),
                 (1, 1, 0, 0),
                 (1, 1, 1, 1)]

swap :: WireMap -> String -> String -> WireMap
swap wm a b =
  let va = fromJust $ Map.lookup a wm in
  let vb = fromJust $ Map.lookup b wm in
  (Map.insert a vb . Map.insert b va) wm

findLoopsFromKey :: WireMap -> String -> Bool
findLoopsFromKey wm k =
  go k Set.empty
    where
      go k s
        | Set.member k s = True
        | otherwise = 
          let s' = Set.insert k s in
          case Map.lookup k wm of
            Just (AndGate a b) -> go a s' || go b s'
            Just (OrGate a b) -> go a s' || go b s'
            Just (XorGate a b) -> go a s' || go b s'
            _ -> False

findLoops :: WireMap -> Bool
findLoops wm = any (findLoopsFromKey wm) (Map.keys wm)

-- Check the nth bit. If it doesn't pass all tests, swap bits
-- until it does, and return a modified WireMap. Candidates for
-- swapping are just the gates that are new with this bit, since
-- we assume that previous bits have already been checked and
-- fixed. There are typically only 5 gates per bit. We try swapping
-- each of them with all unresolved gates (i.e. gates that were not 
-- used for previously checked bits). The assumption here is that
-- only one of the new gates will need swapping. This may not be 
-- true in general, but is true for our input at least.
fixBit :: WireMap -> Int -> WireMap
fixBit wm n
  | checkBit wm n = wm
  | otherwise =
    let zkeys = wireKeys wm "z" in
    let checkedKeys = deps wm (take n zkeys) in
    let uncheckedKeys = Set.difference (Map.keysSet wm) checkedKeys in
    let depKeys = deps wm (take (n + 1) zkeys) in
    let newKeys = Set.difference depKeys checkedKeys in
    head [m | a <- Set.toList newKeys, 
              b <- Set.toList uncheckedKeys,
              let m = swap wm a b,
              not (findLoops m),
              checkBit m n]

-- Find all the keys that have different values, assuming
-- that both maps have the same keys
findDiffs :: WireMap -> WireMap -> [String]
findDiffs a b = 
  let ma = Map.assocs a in
  let mb = Map.assocs b in
  [ka | ((ka, va), (kb, vb)) <- zip ma mb, va /= vb]

part1 wm = show . load (wireKeys wm "z") $ wm
part2 wm = 
  let wm' = foldl fixBit wm [0..44] in
  intercalate "," . sort $ findDiffs wm wm'

run = Aoc.run 24 parse part1 part2

