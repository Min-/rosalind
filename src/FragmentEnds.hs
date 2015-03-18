{-#LANGUAGE OverloadedStrings#-}

-- 12-4-2014
-- make mm9 individual fragment ends (blocks contain HindIII) as a gtf file

import qualified Data.ByteString.Char8 as B
import qualified Data.Text.IO as TextIO
import qualified Data.Text as T
import qualified System.Process

chr10 = "/Users/minzhang/Documents/data/P33_enhancer_4C/newruns_112214/FourSeq/mm9/chr10.fa"::FilePath
chr10upper = "/Users/minzhang/Documents/data/P33_enhancer_4C/newruns_112214/FourSeq/mm9/chr10upper.txt"::FilePath
chr10gtf = "/Users/minzhang/Documents/data/P33_enhancer_4C/newruns_112214/FourSeq/mm9/chr10HindIII.gtf"::FilePath

wouterChr10 = "/Users/minzhang/Documents/data/P33_enhancer_4C/newruns_112214/FourSeq/mm9/wouter_chr10_reducedGenomeHindIIIDpnII.txt"::FilePath
wouterChr10Output = "/Users/minzhang/Documents/data/P33_enhancer_4C/newruns_112214/FourSeq/mm9/wouter_chr10_reducedGenomeHindIIIDpnII.gtf"::FilePath



hindIII = "AAGCTT"::B.ByteString

main = do
    printHindIIIpos
--  index <- TextIO.readFile "temp.txt" >>= return . T.lines
    index <- TextIO.readFile wouterChr10 >>= return . map (head . T.splitOn " ") . T.lines
--  let output = makeGtf "chr10\t" "mm9_ref\t" "fragment_end\t" index 100 ""
    let output = makeGtf "chr10\t" "mm9_ref\t" "fragment_end\t" index 50 ""
--  TextIO.writeFile chr10gtf (T.unlines output)
    TextIO.writeFile wouterChr10Output (T.unlines output)
--  System.Process.system "rm temp.txt"
  
  

indicesOfEnzyme enzyme fa = B.findSubstrings enzyme fa
numberOfEnzymes enzyme fa = length $ indicesOfEnzyme enzyme fa

makeGtf seqname source feature ind halfWindow attr = map addFeatures ind 
  where addFeatures i = T.concat [seqname, source, feature, interval i, score, strand, frame, attribute i]
        score = "0\t"
        strand = ".\t"
        frame = ".\t"
        interval x = T.concat [start x, "\t", end x, "\t"]
        start x = T.pack $ show $ (read (T.unpack x)::Int) - halfWindow
        end x = T.pack $ show $ (read (T.unpack x)::Int) + halfWindow
        attribute x = T.concat ["fragment_end_id ", attr, x, ";"]
 
printHindIIIpos = do
 -- chr10fa <- fmap (T.toUpper . T.intercalate "" . tail . T.lines) $ TextIO.readFile chr10
 -- TextIO.writeFile chr10upper chr10fa
  chr10UpperCase <- B.readFile chr10upper 
  B.writeFile "temp.txt" $ B.unlines $ map (B.pack . show) $ indicesOfEnzyme hindIII chr10UpperCase

-- some nt are in lower cases
--chr10faSeq = fmap (B.take 15 . B.drop 62920985 . B.intercalate "" . tail . B.lines) $ B.readFile chr10


