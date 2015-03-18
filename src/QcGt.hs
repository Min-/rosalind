{-#LANGUAGE OverloadedStrings#-}

module QcGt
where

import DataTypes
import Barcodes (matchBC, filterMatchedReads)
import ParseTags

import qualified Data.Text as T
import qualified Data.Maybe as M
import qualified Data.List as L

-- remove this function from Barcodes module, expand it with more options
-- findbcsample in julia, get cigar or md code from each barcoded sample
getSampleTags rs bc1 bc2 m 
  | m == Cigar = Just (map (\r->cigar r) matchedReads)
  | m == Md = Just (map (\r->md r) matchedReads)
  | m == Count = Just (map (\r->"1") matchedReads)
  | m == Seq = Just (map (\r-> samseq r) matchedReads)
  | m == Flag = Just (map (\r-> T.pack $ show (flag r)) matchedReads)
  | m == Len = Just (map (\r-> T.pack $ show $ T.length $ samseq r) matchedReads)
  | otherwise = Nothing 
     where matchedReads = filterMatchedReads rs bc1 bc2 


-- s for sam
generateSeqLength s bc1 bc2 = (groupCount . M.fromJust) $ getSampleTags s bc1 bc2 Len

generateMd s bc1 bc2 = (convertMdTag . M.fromJust) $ getSampleTags s bc1 bc2 Md

generateCigar s bc1 bc2 = (convertCigarTag . M.fromJust) $ getSampleTags s bc1 bc2 Cigar
-- test if we can get even length of tags
--generate3Cigar s bc1 bc2 = (groupCount . map (\x->T.take 2 x) . M.fromJust) $ getSampleTags s bc1 bc2 Cigar
generateMergedTags s bc1 bc2 = mapMergeTags (generateCigar s bc1 bc2) (generateMd s bc1 bc2)

generateMergedTagsLength s bc1 bc2 = groupCount $ map (T.pack . show . T.length) $ generateMergedTags s bc1 bc2 

generateMergedTagsAveLength s bc1 bc2 = T.pack $ show $ average $ map T.length $ generateMergedTags s bc1 bc2
  where average xs = realToFrac (sum xs)/ L.genericLength xs

-- get sample sequence to see mutations
generateSampleSequence s bc1 bc2 = showSampleSeq $ M.fromJust $ getSampleTags s bc1 bc2 Seq
  where  showSampleSeq s
          | null s = "no reads"
          | otherwise = T.intercalate "\n" $ take (length s `div` 10) s

--get minimum length of 95% reads
generateMergedMinLength s bc1 bc2 = headSafe $ drop fivePctNumber $ L.sort $ map T.length $ generateMergedTags s bc1 bc2
  where totalNumber = realToFrac $ length $ generateMergedTags s bc1 bc2
        fivePctNumber = ceiling $ 0.05 * totalNumber
        headSafe x 
          |x==[] = Nothing
          |otherwise = Just (head x) 
      
generateMapping s bc1 bc2 = (groupCount . M.fromJust) $ getSampleTags s bc1 bc2 Flag

-- reverse to put "_" first
groupCount = map (\x->[head x, T.pack $ show $ length x]) . L.group . L.reverse . L.sort
groupPct l = map (\x->[head x, T.pack $ show $ (L.genericLength x/L.genericLength l)]) . L.group . L.reverse. L.sort $ l
        

