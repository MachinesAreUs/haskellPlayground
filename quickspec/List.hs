import Test.QuickSpec hiding (lists)
import Test.QuickCheck

lists = [
  ["xs","ys","zs"] `vars` (undefined :: [A]),
  
  background [ 
    "[]"      `fun0` ([] :: [A]),
    "reverse" `fun1` (reverse :: [A] -> [A]),
    "++"      `fun2` ((++) :: [A] -> [A] -> [A]),
    "+"       `fun2` ((+) :: Int -> Int -> Int)
  ],
  "length"  `fun1` (length :: [A] -> Int)
    
  ] 

main = quickSpec lists




