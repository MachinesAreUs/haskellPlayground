module GameOfLife
(  newUniverse, evolve
  , Cell(..), Universe(..)
  , evolveRow, evolveCell, toInt
) where

import Data.List

data Cell = O | X deriving (Eq, Show) -- O=Dead, X=Live
data Universe = Universe { width :: Int, 
                           height :: Int,
                           rows :: [[Cell]]
                         } deriving (Show)

evolve :: Universe -> Universe
evolve universe = foldl evolveRow universe [0..(height universe - 1)]

evolveRow :: Universe -> Int -> Universe
evolveRow universe idx =
	let cells = rows universe
	    row = cells !! idx
	    prev = if idx == 0 then last cells else cells !! (idx - 1)
	    next = if idx == (height universe - 1) then head cells else cells !! (idx + 1)
	    allPrev = fst . splitAt idx $ cells
	    allNext = tail . snd . splitAt idx $ cells
	    newRow = map evolveCell $ neighborhoods4CellsInRow prev row next
	in Universe { width = width universe,
	              height = height universe, 
	              rows = allPrev ++ newRow:allNext }

neighborhoods4CellsInRow :: [Cell] -> [Cell] -> [Cell] -> [[[Cell]]]
neighborhoods4CellsInRow prevRow row nextRow =
	let columns = zipWith3 (\x y z -> [x,y,z]) prevRow row nextRow
	    nextNeighborhood = (\idx ->  transpose . take 3 . drop idx . cycle $ columns) 
	    unordered = map nextNeighborhood [0..(length columns - 1)]
	in last unordered : init unordered 

evolveCell :: [[Cell]] -> Cell
evolveCell neighborhood@((_:_:_:[]):(_:cell:_:[]):(_:_:_:[])) 
	| cell == X && livingNeighbors `elem` [2,3] = X
	| cell == O && livingNeighbors == 3 = X
	| otherwise = O
	where livingNeighbors = (sum . map toInt . concat $ neighborhood ) - (toInt cell)

newUniverse :: [[Cell]] -> Universe	
newUniverse rows@(r1:r2:r3)
	| length r1 >= 3 && allRowsSameSize 
		= Universe {width = length r1, height = length rows, rows = rows }
	| otherwise = error "Every row must have the same number of columns, with n >= 3"
	where allRowsSameSize = and . map (\r -> length r == length r1) $ rows
	
toInt :: Cell -> Int
toInt O = 0
toInt X = 1