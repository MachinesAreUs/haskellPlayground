import Data.Maybe
import Data.List
import qualified Data.Map as Map  

data Toy = Buzz | Woody | Rex | Hamm deriving (Eq, Ord, Show)
type Movement = (Toy, Toy)
type Solution = [Movement]

time :: Toy -> Int
time Buzz  = 5
time Woody = 10
time Rex   = 20
time Hamm  = 25

timeForward :: Movement -> Int
timeForward m = max (time (fst m)) (time (snd m))

timeBackward :: Movement -> Int
timeBackward m = time (fst m) 

timing :: Solution -> Int
timing []     = 0
timing (m:[]) = timeForward m
timing (m:ms) = timeForward m + timeBackward m + timing ms
        
choices :: [Toy] -> [Movement]
choices []   = []
choices toys = filter (\(a,b) -> a /= b) $ toys >>= \a -> toys >>= \b -> return (a,b)

appendToSolutions :: [Solution] -> Movement -> [Solution]
appendToSolutions []   mov = [[mov]]
appendToSolutions sols mov = map (\sol -> sol ++ [mov] ) sols

solutions :: [Toy] -> [Solution] -> [Solution]
solutions toys@(x:y:[]) sols = appendToSolutions sols (x,y)
solutions toys          sols = concat $ map (completeSolutions sols)  $ choices toys
  where completeSolutions sols = (\mov -> solutions (delete (snd mov) toys) (appendToSolutions sols mov))

zurg = fastest $ map (\s -> (s, timing s)) $ solutions [Buzz, Woody, Rex, Hamm] []
  where fastest = minimumBy (\x y -> compare (snd x) (snd y))