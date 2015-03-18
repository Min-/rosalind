{-#LANGUAGE OverloadedStrings#-}

module Primers
where

import IO 
import Barcodes
import DataTypes
import qualified Data.Text.IO as TextIO
import qualified Data.Text as T

-- use functions from Barcodes, but modified for primer matching
-- including both pure functions and IO functions

-- IO:
-- importPrimers is very similar to importBarcodes, but need primer length longer than 10 nt
-- what if we need to barcode these libraries?
importPairedPrimers = importPairedBarcodes

ioIndividualRegions basePath sam bc n header = do
  let bc' = take n bc
  let result = separateByRegions sam bc'
  let filenames = map (\x-> basePath ++ "/sample"++(show x)++".sam") [1..n]
  let result' = zip result filenames
  mapM_ (\(a, b)->TextIO.writeFile b header >> TextIO.appendFile b "\n" >> outputSamFile a b) result'

-- append file instead of write file
-- do NOT include header
ioIndividualRegions' basePath sam bc n = do
  let bc' = take n bc
  let result = separateByRegions sam bc'
  let filenames = map (\x-> basePath ++ "/sample"++(show x)++".sam") [1..n]
  let result' = zip result filenames
  mapM_ (\(a, b)->outputSamFile a b) result'


ioReadNumber sam bc n = do
  ioQuery quickCheckReadNumber sam bc n 

ioQuery f sam bc n = do
  let bc' = take n bc
  let result = f sam bc'
  mapM_ putBothStr $ zip result [1..n]
    where 
       putBothStr (a, b) = do
         if b < 10 
         then do
           TextIO.putStr "Sample "
           TextIO.putStr (T.pack $ show $ b)
           TextIO.putStr " : "
           TextIO.putStrLn a
         else do
           TextIO.putStr "Sample "
           TextIO.putStr (T.pack $ show $ b)
           TextIO.putStr ": "
           TextIO.putStrLn a



-- pure functions

-- do not filter out mapped reads, because all the reads are not mapped yet.
getIndividualRegions s bc1 bc2 = filterMatchedReads s bc1 bc2

separateByRegions s bcs = map (\x->getIndividualRegions s (fst x) (snd x)) bcs

emptyPrimer = Barcodes "empt" "" Fwd 0 "" "" "" 

quickCheckReadNumber s bcs = map (\x-> T.pack $ show $ length $ filterMatchedReads s (fst x) (snd x)) bcs




