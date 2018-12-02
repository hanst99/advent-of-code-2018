module Main(main)
where

import Problem2

main :: IO ()
main = interact (findFirstNeighbor . lines)
