quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = 
  let lesser = filter (< p) xs 
      greater = filter (>= p) xs
  in (quicksort lesser) ++ [p] ++ (quicksort greater)
  
  