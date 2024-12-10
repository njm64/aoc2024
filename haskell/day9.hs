module Day9 where
import Aoc qualified
import Data.List
import Data.Char
import Data.Ord
import Util

type FileID = Int
type Offset = Int
type Length = Int

type Span = (FileID, Offset, Length)

spanID :: Span -> FileID
spanID (id, _, _) = id

spanOffset :: Span -> Int
spanOffset (_, offset, _) = offset

isFreeSpan :: Span -> Bool
isFreeSpan s = spanID s < 0

parse :: [String] -> [Span]
parse lines =
  go (map digitToInt (head lines)) 0 0 False []
    where go [] _ _ _ bs = bs
          go (n:ns) id i False bs = go ns id (i + n) True ((id, i, n):bs)
          go (n:ns) id i True bs = go ns (id + 1) (i + n) False ((-1, i, n):bs)

getFreeSpans :: [Span] -> [Span]
getFreeSpans = sortOn spanOffset . filter isFreeSpan

getFileSpans :: [Span] -> [Span]
getFileSpans = sortOn (Down . spanOffset) . filter (not . isFreeSpan)

findFreeSpan :: Int -> [Span] -> Maybe Span
findFreeSpan length = find (\(_, _, n) -> n >= length) 

trimSpan :: [Span] -> Span -> Int -> [Span]
trimSpan spans span@(_, i, n) length =
  if n == length then 
    sortOn spanOffset $ delete span spans
  else
    sortOn spanOffset $ delete span ((-1, i + length, n - length):spans)

defrag :: [Span] -> [Span]
defrag spans =
  go (getFreeSpans spans) (getFileSpans spans) []
    where 
      go _ [] rs = rs
      go [] fileSpans processedSpans = fileSpans ++ processedSpans
      go freeSpans fileSpans processedSpans = 
        let (_, freeOffset, freeLength) = head freeSpans in
        let (fileID, fileOffset, fileLength) = head fileSpans in
        if freeOffset > fileOffset then fileSpans ++ processedSpans
        else if fileLength == freeLength 
        then go (tail freeSpans) 
                (tail fileSpans) 
                ((fileID, freeOffset, fileLength) : processedSpans)
        else if fileLength < freeLength 
        then go ((-1, freeOffset + fileLength, freeLength - fileLength):(tail freeSpans)) 
                (tail fileSpans)
                ((fileID, freeOffset, fileLength) : processedSpans)
        else go (tail freeSpans)
                ((fileID, fileOffset, fileLength - freeLength):(tail fileSpans))
                ((fileID, freeOffset, freeLength) : processedSpans)

defrag2 :: [Span] -> [Span]
defrag2 spans = 
  go (getFreeSpans spans) (getFileSpans spans) []
    where
      go _ [] processed = processed
      go xs (f@(fileID, fileOffset, fileLength):fs) processed = 
        case findFreeSpan fileLength xs of 
          Nothing -> go xs fs (f:processed)
          Just free@(_, freeOffset, freeLength) ->
            if freeOffset < fileOffset then
              go (trimSpan xs free fileLength) fs ((fileID, freeOffset, fileLength):processed)
            else 
              go xs fs (f:processed)
            
spanChecksum :: Span -> Int
spanChecksum (id, offset, length) = sum [id * (i + offset) | i <- [0..length - 1]]

checksum :: [Span] -> Int
checksum = sum . map spanChecksum

part1 = checksum . defrag
part2 = checksum . defrag2
run = Aoc.run 9 parse part1 part2



