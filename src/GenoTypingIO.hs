{-#LANGUAGE OverloadedStrings#-}

module GenoTypingIO
where

import GenoTyping
import IO (outputSamFile)
import qualified Data.Text.IO as TextIO
import qualified Data.Text as T

-- high order function for all IO functions and show the results in IonTorrent main module. 
ioQuery f sam bc n = do
  let bc' = getSampleBCs n bc
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

ioQuery' f sam bc n s = do
  let bc' = getSampleBCs n bc
  let result = f sam bc'
  mapM_ nicePrinting $ zip result [1..n]
    where nicePrinting (a, b) = do
            TextIO.putStr "Sample "
            TextIO.putStrLn (T.pack $ show $ b)
            mapM_ (\(result, ref, n)->TextIO.putStr (T.pack $ show n) >> TextIO.putStr "  " >> TextIO.putStr ref >> TextIO.putStr "  : " >> TextIO.putStrLn result) (zip3 (T.splitOn "\n" a) (T.chunksOf 1 s) [1..T.length a]) >> TextIO.putStrLn "\n"

ioQuery'' f sam bc n s = do 
  let bc' = getSampleBCs n bc
  let result = f sam bc'
  mapM_ nicePrinting $ zip result [1..n]
    where   
      nicePrinting (a, b) = do
         TextIO.putStr "Sample "
         TextIO.putStrLn (T.pack $ show $ b)
         TextIO.putStrLn $ T.concat ["      ", "1", T.replicate 9 " ", "11", T.replicate 8 " ", "21", T.replicate 8 " ", "31", T.replicate 8 " ", "41", T.replicate 8 " ", "51", T.replicate 8 " ", "61", T.replicate 8 " ", "71", T.replicate 8 " ", "81", T.replicate 8 " ", "91", T.replicate 8 " "]
         TextIO.putStr "      "
         TextIO.putStrLn s
         TextIO.putStrLn a

-- s is reference sequence
ioQC sam bc n s = do
  printPosNum
  printReferenceSeq s
  ioQuery quickCheckAllSamples sam bc n 

ioReadNumber sam bc n = do
  ioQuery quickCheckReadNumber sam bc n  

ioFullReport sam bc n s = ioQuery' fullCheckAllStats sam bc n s
 
ioSampleSequence sam bc n s = do
  ioQuery'' checkSampleSequence sam bc n s

ioIndividualSample sam bc n header outputPath = do
  let bc' = getSampleBCs n bc
  let result = separateBySamples sam bc'
  let filenames = map (\x-> outputPath ++ "/sample"++(show x)++".sam") [1..n]
  let result' = zip result filenames
  mapM_ (\(a, b)->TextIO.writeFile b header >> TextIO.appendFile b "\n" >> outputSamFile a b) result'
  

printPosNum = TextIO.putStrLn $ T.concat [T.replicate 11 " ", "1", T.replicate 9 " ", "11", T.replicate 8 " ", "21", T.replicate 8 " ", "31", T.replicate 8 " ", "41", T.replicate 8 " ", "51", T.replicate 8 " ", "61", T.replicate 8 " ", "71", T.replicate 8 " ", "81", T.replicate 8 " ", "91", T.replicate 8 " "]

mir17 = T.pack "GGAAGGCCCGTGGTTTATTTTACATCTAACAAAGTGCACTGGCAGATTTGACCAAACTGGAATGGAGAGACCTTGGGGGAGGGGGCGATTTTGTTTTTCTTGTTTTTAAGGGGACCCGCTGCA"

mir92 = T.pack "CGCTTGTAGAGGGCTTCGTGTGCCTGTTGGACCATTAGTTCTTTTAACTGTATATGCAATAACAAGGTTTTAAAAGATAATAATAAAGAGGAAACACGACTATTGGACAA"

yap = T.pack "AGCTGACTGGCTATCCCCTTGCATGCTTGAGTTTTTCAGGACAGTCTTTGTTTCATTTCAGCCATGAACCAGAGGATCACTCAGAGTGCTCCAGTGAAGCAGCCCCCACCCTTGGCTCCCCAGAGCCCACAGGGAGGCGTCCTGGGTGGAGGCAGCTCAAACCAGCAGCAGCAGATACAG"

yap_beta = T.pack "GAAGAAGGAGTCGGGCAGCTTGCGAAGCCGCATGGGCACGGTCTGAGGCACGTTGGCCGTCTTGGGGTTCATGACGGCATTGAAGAGCGCCTCCAAGTCGGTCTCCGAGTCCCCGCGG"
yap_325 = T.pack "AGGCCAGTACTGATGCAGGTACTGCGGGAGCTCTGACTCCACAGCATGTTCGAGCTCACTCCTCTCCAGCCTCCCTGCAGCTGGGTGCCGTTTCTCCTGGGACACTC"
yap_1021 = T.pack "tgtttgtttaagGAATTAGCTCTGCGCAGCCAGTTGCCTACACTGGAGCAGGATGGAGGGACTCCGAATGCAGTGTCTTCTCCTGGGATGTCTCAGGAATTGAGAACAATGACAACCAATAGTTCCGATCCCT"

printReferenceSeq s = TextIO.putStr " Ref Seq : " >> TextIO.putStrLn s

pickSampleBC n = take 1 . drop (n-1)
