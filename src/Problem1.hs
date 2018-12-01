module Problem1
where

parseFreq :: String -> Int
parseFreq ('+':s) = read s
parseFreq s = read s
