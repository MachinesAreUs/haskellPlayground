module GameOfLifeTests
where

import GameOfLife
import Test.QuickCheck

-- Temp

data Board = Board [[Cell]] deriving (Show)

instance Arbitrary Board where
	arbitrary = resize 3 arbitrary

prop_notNull (Board xs) =
	not ( null xs )

-- Generators		

instance Arbitrary Cell where
	arbitrary = elements [X,O]
	
--instance Arbitrary [[Cell]] where
--	arbitrary = resize 3 arbitrary
				   
-- Invariants for evolveCell

prop_deathByOvercrowding neighborhood =
	rightSize neighborhood           ==>
	livingNeighbors neighborhood > 3 ==>
		evolveCell neighborhood == X
	where types = neighborhood::[[Cell]]

-- Invariants for evolveRow


-- Util

rightSize xs =
	length xs == 3 && all (\ys -> length ys == 3) xs

--onlyValidValues xs = all (\x -> x `elem` [1,2]) . concat $ xs

livingNeighbors neighborhood@(_:(_:cell:_:[]):_:[]) = 
	( sum . map toInt . concat $ neighborhood ) - ( toInt cell )