import Criterion.Main as C

qsort1 :: (Ord a) => [a] -> [a]
qsort1 [] = []
qsort1 (x:xs) =
  qsort1 [y | y <- xs, y < x] ++
  [y | y <- xs, y == x] ++
  qsort1 [y | y <- xs, y > x]

main :: IO ()
main = C.defaultMain [
  C.bgroup "qsort1" [ C.bench "1000" $ C.whnf qsort1 [1..1000 :: Int]
                    , C.bench "10000" $ C.whnf qsort1 [1..2000 :: Int]
                    ]
  ]
