{-#LANGUAGE OverloadedStrings#-}

-- interacting version of FourSeq
{-
  Min Zhang
  Martin Lab
  11-24-2014
-}

import qualified Data.Text.IO as TextIO
import qualified Data.Text as T
import Data.List (genericLength)
import System.Environment
import Numeric.GSL.Statistics as Stat
import Data.Packed.Vector as V
import qualified Data.List as L
import Data.Ord (comparing)

import DataTypes
import IO
import Util
import QC
import Primers

main = do
  putStrLn "FourSeqi [inputSamFile] [Option] [OutputPath]" 
  [filepathSam, option, outputPath] <- getArgs
  reads <- importSamFile filepathSam
  runOpt option reads outputPath

runOpt x r out
  |x == "fullReports" = do TextIO.writeFile out $ reports r
  |x == "printFasta" = do outputFasta out $ getUnknownSeqToFasta hindIII dpnII r
  |otherwise = do TextIO.writeFile "./notAnOption.txt" "not an option"

reports r = T.pack $ unlines [title, inputStats, enzymeSiteStats, outputStats]
  where title = "4C-Seq sample processing\n\
                \\n"
        inputStats = unlines [ show $ readStats r
                             , show $ checkFwd r]
        enzymeSiteStats = unlines [ show $ numberOfHindIII r
                                  , show $ numberOfDpnII r
                                  , show $ numberOfHindIIIDpnII r
                                  , show $ readStats $ filterReadsWithBothEnzymes r hindIII dpnII]
        outputStats = show $ length $ getUnknownSeqToFasta hindIII dpnII r
        

readCount = length
readAveLength x = Stat.mean $ V.fromList $ map (fromIntegral . T.length . samseq) x
readStd x = Stat.stddev $ V.fromList $ map (fromIntegral . T.length . samseq) x
readStats x = (readCount x, readAveLength x, readStd x)

checkFwd x = (uniqueBC, uniqueBCNumber)  
  where bc = map (T.take 6 . samseq) x
        uniqueBC = map head $ groupedBC
        uniqueBCNumber = map length $ groupedBC
        groupedBC = L.reverse $ L.sortBy (comparing length) $ L.group $ L.sort $ bc

numberOfRestrictionEnzyme x re = results
  where seq = map samseq x
        reNumber = map (T.count re) seq
        groupedRECount = groupCt reNumber
        results = (map head $ groupedRECount, map length $ groupedRECount)

numberOfHindIII x = numberOfRestrictionEnzyme x hindIII 
numberOfDpnII x = numberOfRestrictionEnzyme x dpnII
numberOfHindIIIDpnII x = numberOfBothEnzymes x hindIII dpnII

numberOfBothEnzymes x re1 re2 = results
  where seq = map samseq x
        reNumber = map (\x-> [T.count re1 x, T.count re2 x]) seq
        groupedRECount = groupCt reNumber
        results = (map head $ groupedRECount, map length $ groupedRECount)

-- 1/12/15 change from 
-- filterReadsWithBothEnzymes x re1 re2 = filter (\x->and [T.count re1 (samseq x) == 1, T.count re2 (samseq x) == 1]) x 

filterReadsWithBothEnzymes x re1 re2 = filter (\x->and [T.count re1 (samseq x) == 1, T.count re2 (samseq x) == 0]) x 

-- two enzyme site count must be [1,1]
trimEnzymeSites re1 re2 seq = map (T.splitOn re2) $ T.splitOn re1 seq 

--getUnknownSeqToFasta re1 re2 r = map reconstructReads qualifiedReads

getUnknownSeqToFasta re1 re2 r = map reconstructReads qualifiedReads
  where qualifiedReads = filterReadsWithBothEnzymes r re1 re2
        reconstructReads r = Fasta (T.concat [">", qname r]) (samseq r)
       -- newseq r = (\[_,b,_]->b) . concat . (trimEnzymeSites re1 re2) . samseq $ r

groupCt x = L.reverse $ L.sortBy (comparing length) $ L.group $ L.sort x
