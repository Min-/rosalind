-- main function

import IO
import Dna
import Barcodes
import ParseTags
import DataTypes
import Qc
import GenoTyping
import GenoTypingIO

import qualified Data.Text as T
import qualified Data.Maybe as M
import qualified Data.List as L
import qualified Data.Text.IO as TextIO
import System.Environment


testDataPath = "/Users/minzhang/Documents/programming/julia/iontorrent/test/yap_mapped_sorted.sam"::FilePath
testDataPath2 = "/Users/minzhang/Documents/data/P30_yan_yap_mut_crispr/raw/mapped.sam"::FilePath
testDataPath3 = "/Users/minzhang/Documents/data/P30_yan_yap_mut_crispr/mir1792/mapped17.sam"::FilePath
testDataPath4 = "/Users/minzhang/Documents/data/P30_yan_yap_mut_crispr/mir1792/mapped92.sam"::FilePath
testDataPath5 = "/Users/minzhang/Documents/data/P30_yan_yap_mut_crispr/101314_betacat_site/yap_crispr_beta_mapped.sam"::FilePath
aurkb = "/Users/minzhang/Documents/data/P12_crispr/aurkb/103014_mice/aurkb.sam"::FilePath

barcodePath = "/Users/minzhang/Documents/private_git/hCommands/IonTorrent/src/aurkb_set2.txt"::FilePath
barcodePath2 = "/Users/minzhang/Documents/private_git/hCommands/IonTorrent/src/testbc1792.txt"::FilePath



main = do
  [inputSam, barcodePath, outputPath, n] <- getArgs
  sam <- importSamFile inputSam
  bc <- importPairedBarcodes barcodePath
  header <- keepHeader inputSam
  ioIndividualSample sam bc (read n::Int) header outputPath
 -- ioQC sam bc 64 yap_1021
