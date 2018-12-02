import Problem2

main :: IO ()
main = interact (show . checksum . foldr updateDuplicateCount (DuplicateCount 0 0) . lines)
