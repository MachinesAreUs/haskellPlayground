import Test.QuickSpec hiding (lists)
import Test.QuickCheck

lists = [
  ["xs","ys","zs"] `vars` (undefined :: [A]),
  
  "[]"      `fun0` ([] :: [A]),
  "reverse" `fun1` (reverse :: [A] -> [A]),
  "++"      `fun2` ((++) :: [A] -> [A] -> [A])
  ]

main = quickSpec lists




