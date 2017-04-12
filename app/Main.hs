module Main where

import qualified Fibonacci as Fib
import qualified LongestEdgePathInDAG as LEP

main :: IO ()
main = do
  filepath <- getLine
  longEdgePath <- LEP.longestPath filepath
  putStrLn . show $ longEdgePath
