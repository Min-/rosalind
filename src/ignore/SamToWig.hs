{-#LANGUAGE OverloadedStrings#-}

-- 12/10/14
-- Sam file to WigFile

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Data.Foldable as Fold

import Control.Monad.ST
import Data.STRef
import Control.Monad
import System.Environment


{-- Test Data:
sequence
a = Seq.fromList $ take 400 [False, False ..]

s1 = [1, 10]::[Int]
s2 = [10, 20]
s3 = [15, 21]

allReads = [s1, s2, s3]
-}

-- functions:

buildEmptyWig species chr = 
  case species of "mm9" -> buildMouse chr
                  "hg19" -> buildHuman chr
  where buildMouse chr
          |chr=="chr1" = buildChr 197195432
          |chr=="chr10" = buildChr 129993255
          |chr=="chr11" = buildChr 121843856
          |chr=="chr9" = buildChr 124076172
          |otherwise = undefined
        buildChr n =  Seq.fromList $ take n [0, 0 ..]
        buildHuman = undefined

-- reducedSam is different from original Sam file, because of the "Reduced reference genome format"
reducedSamToReadList sam = do
  s' <- TextIO.readFile sam >>= return . filter (\x->x/="*") . map (head . drop 2 . T.splitOn "\t") . filter (\x->T.head x /='@') . T.lines
  let interval = map (\x->T.splitOn "-" $ last $ T.splitOn ":" x) s'
  let interval' = map (\[a,b]->[read (T.unpack a)::Int, read (T.unpack b)::Int]) interval
  return interval'
       
singleReadToWig [s, e] initSeq = runST $ do
  wig <- newSTRef initSeq
  let i = [(s - 1) .. (e - 1)] in
    forM_ i $ \x-> do
      modifySTRef wig (\s->(Seq.adjust (\b-> or' [1, b]) x s))
  readSTRef wig

listReadToWig sequence allReads = runST $ do
  wig <- newSTRef sequence  
  forM_ allReads $ \x->do
    modifySTRef wig (\r-> singleReadToWig x r) 
  readSTRef wig

or' x
  |x==[1, 0] = 1
  |x==[1, 1] = 1
  |x==[0, 1] = 1
  |otherwise = 0

main = do
  putStrLn "\n SamToWig [sam] [species] [chr] [outputWig] \n"
  [sam, species, chr, outputWig] <- getArgs 
  let sequence = buildEmptyWig "mm9" chr
  intraChrReads <- reducedSamToReadList sam
  let wigVarStepLine = "variableStep\tchrom=" ++ chr ++ "\tspan=1\n"
  let wigBody = unlines $ map (\(a,b)->show a ++ "\t" ++ show b) $ zip [1..] (Fold.toList $ listReadToWig sequence intraChrReads) 
  let result = concat [wigVarStepLine, wigBody]
  writeFile outputWig result
  
  
  
  
  
