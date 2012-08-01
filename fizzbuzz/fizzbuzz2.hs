fizzbuzz x = let threes = cycle ["","","fizz"]
                 fives  = cycle ["","","","","buzz"]
             in take x $ zipWith (++) threes fives 
		   
main = mapM_ putStrLn $ fizzbuzz 100