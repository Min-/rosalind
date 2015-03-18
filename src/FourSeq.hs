{-#LANGUAGE OverloadedStrings#-}

{-main function for 4C-Seq data analysis

  Min Zhang
  Martin Lab
  9-29-2014
-}

import qualified Data.Text.IO as TextIO
import qualified Data.Text as T
import System.Environment

import DataTypes
import IO
import Util
import QC
import Primers

--filepathSam = "/Users/minzhang/Documents/data/P33_enhancer_4C/newruns_102114/IonXpressRNA_011_R_2014_10_17_14_50_43_user_JFM-43-Yang_nonCM_rnaseq_101714_Auto_user_JFM-43-Yang_nonCM_rnaseq_101714_92.bam.sam"::FilePath
--filepathFastq = "/Users/minzhang/Documents/data/P33_enhancer_4C/4c_fst_092514/4c_raw.fastq"
--filepathPrimers = "/Users/minzhang/Documents/private_git/hCommands/FourSeq/src/4cPrimers.txt"::FilePath

main = do
  putStrLn "FourSeq [inputSamFile] [BarcodeTxt] [baseDir]"
  [filepathSam, filepathPrimers, basedir] <- getArgs
  reads <- importSamFile filepathSam
  primers <- importBarcodeTxt filepathPrimers
  let primerpairs = zip primers (replicate (length primers) emptyPrimer)
  header <- keepHeader filepathSam
  --ioReadNumber reads primerpairs 40
  -- ioIndividualRegions' does not include header lines for each Sam file
  ioIndividualRegions' basedir reads primerpairs 40 
