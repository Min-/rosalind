{-#LANGUAGE OverloadedStrings#-}

{-main function for 4C-Seq data analysis
  same as FourSeq, instead use single sam file, use a sam input folder and map the function to individual splitted files

  Min Zhang
  Martin Lab
  9-29-2014
-}

import qualified Data.Text.IO as TextIO
import qualified Data.Text as T
import System.Environment
import System.Directory
import Control.Monad (forM_)

import DataTypes
import IO
import Util
import QC
import Primers

main = do
  putStrLn "FourSeq [inputSamFileFolder] [BarcodeTxt] [baseDir]"
  [filepathSamFolder, filepathPrimers, basedir] <- getArgs
  setCurrentDirectory filepathSamFolder
  filepathSams <- getDirectoryContents filepathSamFolder >>= return . filter (\x->and [x/=".", x/=".."]) 
  print filepathSams
  forM_ filepathSams (\x-> do
    reads <- importSamFile x
    primers <- importBarcodeTxt filepathPrimers
    let primerpairs = zip primers (replicate (length primers) emptyPrimer)
    -- ioIndividualRegions' does not include header lines for each Sam file
    ioIndividualRegions' basedir reads primerpairs 40)

