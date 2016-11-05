module Fibonacci
    ( test
    ) where

import qualified Data.Matrix as Mat

test :: IO ()
test = do
  putStrLn "I will give you fibonacci of N : give me N : "
  n <- read <$> getLine
  putStrLn . show . nthFib $ n

type IntType = Integer

fibMatrix :: Mat.Matrix IntType
fibMatrix = Mat.fromLists [ [0, 1]
                          , [1, 1] ]

-- should mempty be identity
raiseToN :: (Num a) => Mat.Matrix a -> Int -> Mat.Matrix a
raiseToN matrix n =
  case n of
    0 -> Mat.identity (Mat.nrows matrix)
    1 -> matrix
    _ ->
      let baseMatrix = if n `mod` 2 == 0
                       then Mat.identity (Mat.nrows matrix)
                       else matrix
          raiseToNBy2 = raiseToN matrix (n `div` 2)
      in Mat.multStd (Mat.multStd raiseToNBy2 raiseToNBy2) baseMatrix

nthFib :: Int -> Maybe IntType
nthFib n =
  let nthFibMatrix = raiseToN fibMatrix n
  in Mat.safeGet 1 1 nthFibMatrix
