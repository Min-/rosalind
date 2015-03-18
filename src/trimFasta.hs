{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  1/9/2014
  Trim filtered fasta files for at least n bp long reads
-}

import IO
import DataTypes

import qualified Data.Text as T

main = do
  putStrLn "Enter input file path: "
  input <- getLine
  putStrLn "Enter minimum length of the reads: "
  n <- fmap readInt getLine
  let outputPath= input ++ ".trim.fasta"
  fa <- fmap (filter (trimfa n)) (importFasta input)
  outputFasta outputPath fa

trimfa n fa = (T.length (faseq fa)) >= n



readInt :: String -> Int
readInt = read
