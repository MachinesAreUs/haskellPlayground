module KnightsTour where

import Data.List
import Control.Parallel.Strategies

-- Auxiliary Functions

type Position = (Int,Int)
type Solution = [Position]

possibleMovs :: Int -> Position -> [Position]
possibleMovs n (r,c)            = sort movs
  where movs                    = [(r',c') | r' <- [r-2..r+2], c' <- [c-2..c+2], isValid n (r,c) (r',c')]
        isValid n (r,c) (r',c') = (isWithinBoard n r' c') && (abs(r-r') + abs(c-c') == 3)
        isWithinBoard n r' c'   = r' >= 1 && r' <= n && c' >= 1 && c' <= n

appendToSolutions :: [Solution] -> Position -> [Solution]
appendToSolutions []   mov = [[mov]]
appendToSolutions sols mov = map (\sol -> sol ++ [mov] ) sols

tour :: Int -> Position -> [Solution] 
tour n from                   = tour' n from [[from]]
  where tour' n from sols     = filter (\s -> length s == n*n) $ concat $ solutions n from sols
        solutions n from sols = parMap rpar (completeSols sols) $ possibleMovs n from 
        completeSols sols     = \m -> if (isNewPosition m sols ) 
                                      then tour' n m $ appendToSolutions sols m
                                      else sols
        isNewPosition m sols = all (\sol -> elemIndex m sol == Nothing) sols

knightsTour n          = filter (\s -> length s /= 0) $ concat $ parMap rpar (tour n) allPositions
  where allPositions   = [(r,c) | r <- [1..n], c <- [1..n]]


