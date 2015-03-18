{-#LANGUAGE OverloadedStrings#-}

-- Statistics on mapped Sam file (inter-, intra-chromosome; distance to pointviews; count anchor points)

{-
  Min Zhang
  11-29-2014
  Martin lab
-}

import DataTypes
import IO (importSamFile)
import Util (groupCount, groupPct, binNumbers)

import qualified Data.Text as T
import qualified Data.List as L
import System.Environment

main = do
  [samPath, chrom, startSite] <- getArgs 
  sam <- importSamFile samPath
  putStrLn "chromosome stats"
  print $ statsChrom sam 
  putStrLn "mapping stats"
  print $ statsMappingFlag sam
  putStrLn "distance to viewpoint"
  print $ distanceIntraChrom sam (T.pack chrom) (read startSite::Int)
  putStrLn "numbers of reads"
  print $ length sam
  putStrLn "average read length"
  print $ fragmentLen sam

fragmentLen sam = (fromIntegral $ sum $ map (T.length . samseq) sam) / (L.genericLength sam)

statsChrom sam = groupPct $ map rname sam
statsMappingFlag sam = groupPct $ map (T.pack . show . flag) sam

distanceIntraChrom sam chr start
  |topChr == chr = show $ calculateDist sam chr start 
  |topChr /= chr = "Primary mapped chromosome is not the selected chromosome."
  |otherwise = "Not an option."
  where topChr = fst $ head $ statsChrom sam 
        calculateDist sam chr start = binDist $ map (\r -> abs (start - (pos r))) (intraChromReads sam chr)
        intraChromReads sam chr = filter (\x->rname x == chr) sam
        binDist = binNumbers

uniqueReads sam = groupCount $ map (\x-> (rname x, pos x)) sam
  
