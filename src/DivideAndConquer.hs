module DivideAndConquer where

kthSmallest :: (Ord a) => Int -> [a] -> Maybe a
kthSmallest k list =
  if k < 0
  then Nothing
  else
    case list of
      [] -> Nothing
      x:xs ->
        let pivot = x -- randomness would have helped
            smaller = filter (< x) list
            same = filter (== x) list
            larger = filter (> x) list
            lenSmaller = length smaller
            lenSame = length same
        in if lenSmaller > k
           then kthSmallest k smaller
           else
             if lenSmaller + lenSame > k
             then Just x
             else kthSmallest (k - lenSmaller - lenSame) larger
