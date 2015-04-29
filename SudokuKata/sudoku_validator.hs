import System.Environment
import Data.List
import Data.Char

(|>) = flip ($)

type Matrix = [[Int]]

isValidSolution :: String -> Bool
isValidSolution s = isValidInput && isValid
  where isValid      = s |> toMtx |> isValidSolution_
        isValidInput = s |> map isDigit |> and 

isValidSolution_ :: Matrix -> Bool
isValidSolution_ mtx = map ($ mtx) [rowsValid, columnsValid, squaresValid] |> and
  where rowsValid m         = m |> map isValidSequence |> and
        columnsValid m      = m |> transpose |> rowsValid 
        squaresValid m      = squares m |> map isValidSequence |> and
        isValidSequence seq = seq |> nub |> sort |> (==canonical) 
        squares m           = [[at m r c | r <- rr, c <- cr] | rr <- ranges, cr <- ranges ]
        ranges              = [[0..2],[3..5],[6..8]]
        canonical           = [1..9] --:: [Int]

toMtx :: String -> Matrix
toMtx s = [[ elem row col | col <- [0..size-1]] | row <- [0..size-1]]
  where chars = filter (/=' ') s
        size  = truncate $ sqrt $ fromIntegral $ length chars
        elem r c = digitToInt $ head $ drop (r * size + c) chars

at :: Matrix -> Int -> Int -> Int 
at mtx r c = (mtx !! r) !! c 

main = do
  argv <- getArgs
  fileStr <- readFile $ head argv
  let samples = lines fileStr 
  mapM (putStrLn . show . isValidSolution) samples 
