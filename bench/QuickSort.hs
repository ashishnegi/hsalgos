-- module QuickSort where
import Criterion.Main as C
import Data.List as DL
import System.Random as R

qsortSimple :: (Ord a) => [a] -> [a]
qsortSimple [] = []
qsortSimple (x:xs) =
  qsortSimple [y | y <- xs, y < x] ++ [x] ++
  qsortSimple [y | y <- xs, y >= x]

qsortFoldr :: (Ord a) => [a] -> [a]
qsortFoldr [] = []
qsortFoldr (x:xs) = (qsortFoldr smaller) ++ same ++ (qsortFoldr greater)
  where
    (smaller, same, greater) = foldr (\v (sm,sa,gr) ->
                                        if v < x
                                        then (v:sm, sa, gr)
                                        else if v == x
                                        then (sm, v:sa, gr)
                                        else (sm, sa, v:gr))
                               ([],[x],[])
                               xs

qsortFoldl :: (Ord a) => [a] -> [a]
qsortFoldl [] = []
qsortFoldl (x:xs) = qsortFoldl smaller ++ same ++ qsortFoldl greater
  where
    (smaller, same, greater) = foldl (\(sm,sa,gr) v ->
                                        if v < x
                                        then (v:sm, sa, gr)
                                        else if v == x
                                        then (sm, v:sa, gr)
                                        else (sm, sa, v:gr))
                               ([],[x],[])
                               xs

qsortFoldl' :: (Ord a) => [a] -> [a]
qsortFoldl' [] = []
qsortFoldl' (x:xs) = qsortFoldl' smaller ++ same ++ qsortFoldl' greater
  where
    (smaller, same, greater) = DL.foldl' (\(sm,sa,gr) v ->
                                        if v < x
                                        then (v:sm, sa, gr)
                                        else if v == x
                                        then (sm, v:sa, gr)
                                        else (sm, sa, v:gr))
                               ([],[x],[])
                               xs

qsortBench :: ([Int] -> [Int]) -> [Int] -> [C.Benchmark]
qsortBench qsort nums = [ C.bench (show (length nums)) $ C.nf qsort nums ]

main :: IO ()
main = do
  r <- R.getStdGen
  let nums = (take 5000 $ R.randoms r) :: [Int]
  C.defaultMain [ C.bgroup "qsortFoldl'" $ qsortBench qsortFoldl' nums
                , C.bgroup "qsortFoldr" $ qsortBench qsortFoldr nums
                , C.bgroup "qsortFoldl" $ qsortBench qsortFoldl nums
                , C.bgroup "qsortSimple" $ qsortBench qsortSimple nums
                ]
  print $ show ((qsortSimple nums == qsortFoldr nums) &&
                (qsortFoldr nums == qsortFoldl nums) &&
                (qsortFoldl nums == qsortFoldl' nums))

  -- print $ nums
  -- print $ qsortSimple nums
  -- print $ qsortFoldr nums
  -- print $ qsortFoldl nums
  -- print $ qsortFoldl' nums
