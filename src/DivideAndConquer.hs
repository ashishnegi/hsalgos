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
            (smaller, same, larger) = listInParts x list
            lenSmaller = length smaller
            lenSame = length same
        in if lenSmaller > k
           then kthSmallest k smaller
           else
             if lenSmaller + lenSame > k
             then Just x
             else kthSmallest (k - lenSmaller - lenSame) larger

listInParts :: (Ord a) => a -> [a] -> ([a],[a],[a])
listInParts x list =
  case list of
    [] -> ([],[],[])
    y:ys ->
      let (smaller, same, larger) = listInParts x ys
      in if y < x
         then (y:smaller, same, larger)
         else
           if y == x
           then (smaller, x:same, larger)
           else (smaller, same, y:larger)
