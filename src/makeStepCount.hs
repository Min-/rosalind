{-
  Min Zhang
  1/10/15
  Make step wise count based on a input wig file in a defined region.

  Example:
  input: [[1, 3.4], [2, 5], [3, 8.4], [7, 1.2], [8, 2]]
  output: [[1, 3.4], [2, 5], [3, 8.4], [4, 0], [5, 0], [6, 0], [7, 1.2], [8, 2], [9, 0], [10, 0]]
  
-}

import Data.Function (on)
import Data.List (foldl', sortBy, group, groupBy)
import Data.Ord (comparing)
import Control.Applicative

exampleInput =  [(1, 3.4), (2, 5), (3, 8.4), (7, 1.2), (8, 2)]
exampleIndex = zip [1..10] [0::Double, 0 ..]

makeSeq start step = [start, start+step ..]

merge a b = map addAll $ groupBy ((==) `on` fst) $ sortBy (comparing fst) $ a ++ b

addAll xs = ((fst . head) xs, sum $ map snd xs) 

toPair [a, b] = (readInt a, readDouble b)

readInt :: String -> Int
readInt = read
readDouble :: String -> Double
readDouble = read

printPair = unlines . map (unwords . (\(a,b)->[show a, show b]))

main = do
  putStrLn "Enter input file path: "
  inputPath <- getLine
  let outputPath = inputPath ++ "merged.txt"
  putStrLn "Enter start position: "
  start <- readInt <$> getLine
  putStrLn "Enter end position: "
  end <- readInt <$> getLine
  input <- readFile inputPath >>= return . map (toPair . words) . lines
  let index = zip [start, start + 25 .. end] [0::Double, 0 ..]
  let result = merge input index
  writeFile outputPath (printPair result)
  
  

