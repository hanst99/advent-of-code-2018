{-# LANGUAGE OverloadedStrings #-}
module Main(main)
where

main :: IO ()
main = interact (show . sum . map read . lines)
