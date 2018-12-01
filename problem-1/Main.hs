{-# LANGUAGE OverloadedStrings #-}
module Main(main)
where

import Problem1

main :: IO ()
main = interact (show . sum . map parseFreq . lines)
