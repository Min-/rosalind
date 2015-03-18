{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  01/12/15 clean up the reads with only one enzyme site HindIII or DpnII

  reverse the reads with reversed barcode;
  clean up short reads: total lenght <= length barcode (primer) + 50 bp
  trim barcode;
-}

import IO
import DataTypes
import Dna

import qualified Data.Text as T
import Control.Applicative

main = do
  (input, output, n, head5) <- welcome
  fa <- importFasta input 
  outputFasta output (process n head5 fa)

welcome = do
  putStrLn "Enter input fasta file: "
  fastaPath <- getLine
  putStrLn "Enter barcode length: "
  bcLength <- readInt <$> getLine
  putStrLn "Enter first 5 nucleotides of the barcode: "
  bcFirstFive <- T.pack <$> getLine
  return (fastaPath, fastaPath ++ ".cleanup.fasta", bcLength, bcFirstFive)

readInt :: String -> Int
readInt = read

process n head5 = map (trimPrimer n) . map (reverseFa head5) . (filterShortFa n)

filterShortFa n = filter (\x->T.length (faseq x) >= n + 50)

reverseFa h fa = if T.take 5 (faseq fa) == h
                 then fa
                 else Fasta (faname fa) (revCompSeq $ faseq fa)

trimPrimer n fa = Fasta (faname fa) (T.drop n $ faseq fa)
