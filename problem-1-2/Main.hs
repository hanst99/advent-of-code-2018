import Problem1
import qualified Data.IntSet as S

findFirstDuplicate :: [Int] -> Int
findFirstDuplicate = go 0 (S.singleton 0) where
  go prevFreq knownFreqs (freqDiff:freqDiffs)
    | S.member newFreq knownFreqs = newFreq
    | otherwise = go newFreq (S.insert newFreq knownFreqs) freqDiffs where
    newFreq = prevFreq + freqDiff
  -- this shouldn't happen as the input list should always be infinite
  go _ _ []  = error "didn't find any duplicate frequencies"

main :: IO ()
main = interact (show . findFirstDuplicate . cycle . map parseFreq . lines)
