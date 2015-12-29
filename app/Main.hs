module Main where

import Lib

main :: IO ()
main = putStr (render_to_pgm 500 500)
