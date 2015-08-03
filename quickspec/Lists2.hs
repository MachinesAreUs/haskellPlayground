import Test.QuickSpec hiding (lists)

lists = [
  prelude (undefined :: A) `without` ["[]",":"],
  
  background [
    "reverse" `fun1` (reverse :: [A] -> [A])
    ],
  "length" `fun1` (length :: [A] -> Int)
  ]

main = quickSpec lists
