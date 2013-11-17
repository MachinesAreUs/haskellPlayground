module KnightsTour where

import Data.List
import Control.Parallel.Strategies

type Position = (Int,Int)
type Solution = [Position]

possibleMovs :: Int -> Position -> [Position]
possibleMovs n (r,c)          = [(r',c') | r' <- [r-2..r+2], 
                                           c' <- [c-2..c+2], 
                                           isLMovement (r',c'),
                                           isWithinBoard (r',c')]
  where isLMovement (r',c')   = abs(r-r') + abs(c-c') == 3
        isWithinBoard (r',c') = r' >= 1 && r' <= n && c' >= 1 && c' <= n

appendToSolutions :: [Solution] -> Position -> [Solution]
appendToSolutions []   mov = [[mov]]
appendToSolutions sols mov = map (\sol -> sol ++ [mov] ) sols

tours :: Int -> Position -> [Solution] 
tours n from                  = tours' n from [[from]]
  where tours' n from sols    = filter fullTour $ concat $ solutions n from sols
        solutions n from sols = parMap rpar (completeSols sols) $ possibleMovs n from 
        completeSols sols     = \m -> if isNewPosition m sols
                                      then tours' n m $ appendToSolutions sols m
                                      else sols
        fullTour              = \s -> length s == n*n     
        isNewPosition p sols  = all (\sol -> elemIndex p sol == Nothing) sols

knightsTour n          = concat $ parMap rpar (tours n) allPositions
  where allPositions   = [(r,c) | r <- [1..n], c <- [1..n]]

