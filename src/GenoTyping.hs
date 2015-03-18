{-#LANGUAGE OverloadedStrings#-}

module GenoTyping
where

import DataTypes
import QcGt (groupPct, generateMergedTags, generateMergedMinLength, generateSampleSequence)
import Barcodes (filterMatchedReads)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Maybe as M (fromJust)

-- turn merged parsed tag into a matrix, and do transpose, need to have uniformed reads, most of the reads are having the same start site, as shown in Qc.hs generate3Cigar function

filterMapped rs = filter (\r->flag r /=4) rs

filterMajority :: [Reads] -> Barcodes -> Barcodes -> [[T.Text]]
filterMajority s bc1 bc2 = map (\t-> T.chunksOf 1 $ T.take minLength t) $ filter (\t ->(T.length t) >= minLength) $ generateMergedTags s bc1 bc2  -- t for merged tag
                             where minLength = safeMinLen $ generateMergedMinLength s bc1 bc2
                                   safeMinLen x
                                     |x==Nothing = 0
                                     |otherwise = M.fromJust x

cleanReads s bc1 bc2 = filterMajority (filterMapped s) bc1 bc2

getIndividualSample s bc1 bc2 = filterMatchedReads (filterMapped s) bc1 bc2

-- ts tag sequence
tagMatrix = map groupPct . L.transpose

-- find mutation locus, s-stats
statMatrix = map findMutant
  where findMutant x
          | null x = "X"
          | and [(head $ head x) == "_", pct >= 0.8] = "-"
          | and [(head $ head x) == "_", pct < 0.8, pct >= 0.45] = T.toLower $ head $ head $ drop 1 x
          | and [(head $ head x) == "_", pct < 0.45] = head $ head $ drop 1 x
          | otherwise = head $ head x 
            where pct = (read $ T.unpack $ last $ head x)::Double

quickCheck :: [[T.Text]] -> T.Text
quickCheck = T.concat . statMatrix . tagMatrix


-- check more information
fullStats = map checkAll
  where checkAll x
          | null x = "X"
          | otherwise = T.intercalate "\t" $ map (T.intercalate " ") x
 
checkFullStats = T.intercalate "\n" . fullStats . tagMatrix


-- which sample to choose, bc here is all posible bc combinations
getBC n bc = let bcpair = head $ drop (n-1) bc
             in (fst bcpair, snd bcpair)

-- get all sample barcodes, a list of barcodes, n is how many samples in the pool
getSampleBCs n bc = let bcpair = take n bc
                    in map (\x->(fst x, snd x)) bcpair

quickCheckAllSamples s bcs = map (\x-> quickCheck $ cleanReads s (fst x) (snd x)) bcs
quickCheckReadNumber s bcs = map (\x-> T.pack $ show $ length $ cleanReads s (fst x) (snd x)) bcs
fullCheckAllStats s bcs = map (\x-> checkFullStats $ cleanReads s (fst x) (snd x)) bcs
checkSampleSequence s bcs = map (\x->generateSampleSequence (filterMapped s) (fst x) (snd x)) bcs
separateBySamples s bcs = map (\x->getIndividualSample s (fst x) (snd x)) bcs

