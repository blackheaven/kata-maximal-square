module Main where

import Square

main :: IO ()
main = interact (show . maximalSquare . lines)
