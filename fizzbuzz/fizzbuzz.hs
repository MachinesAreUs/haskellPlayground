import Data.Char

fizzbuzz x 
  | x `mod` 3 == 0 && x `mod` 5 == 0 = "fizzbuzz"
  | x `mod` 3 == 0 = "fizz"
  | x `mod` 5 == 0 = "buzz"
  | otherwise = show x 

fizzbuzz100 =  
  mapM_ putStrLn  [ fizzbuzz x | x <- [1..100] ]