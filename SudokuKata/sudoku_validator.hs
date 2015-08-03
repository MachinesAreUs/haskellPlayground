import System.Environment
import Data.List
import Data.Char

(|>) = flip ($)

type Matrix = [[Int]]

isValidSolution :: String -> Bool
isValidSolution s    = isValidInput && isValid
  where isValid      = s |> toMtx |> isValidSolution_
        isValidInput = s |> map isDigit |> and 

isValidSolution_ :: Matrix -> Bool
isValidSolution_ mtx        = [rowsValid, columnsValid, squaresValid] |> map ($ mtx) |> and
  where rowsValid m         = m |> map isValidSequence |> and
        columnsValid m      = m |> transpose |> rowsValid 
        squaresValid m      = squares m |> map isValidSequence |> and
        isValidSequence seq = seq |> nub |> sort |> (==canonical) 
        squares m           = [[at m r c | r <- rr, c <- cr] | rr <- ranges, cr <- ranges ]
        ranges              = [[0..2],[3..5],[6..8]]
        canonical           = [1..9] 

toMtx :: String -> Matrix
toMtx s          = [[ at row col | col <- [0..size-1]] | row <- [0..size-1]]
  where chars    = s |> filter (/=' ')
        size     = chars |> length |> fromIntegral |> sqrt |> truncate
        at r c   = chars |> drop (r * size + c) |> head |> digitToInt

at :: Matrix -> Int -> Int -> Int 
at mtx r c = (mtx !! r) !! c 

print :: Matrix -> IO ()
print m = m |> map toLine |> intersperse "\n" |> concat |> putStrLn
  where toLine r = show r

main = do
  argv <- getArgs
  fileStr <- readFile $ head argv
  let samples = lines fileStr 
  mapM (Prelude.print . isValidSolution) samples 
