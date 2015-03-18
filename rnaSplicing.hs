{-#LANGUAGE OverloadedStrings#-}

{->Rosalind_10
ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG
>Rosalind_12
ATCGGTCGAA
>Rosalind_15
ATCGGTCGAGCGTGT-}

import IO
import DataTypes
import RnaToProtein (translate)
import Dna (dnaToRna)
import qualified Data.Text as T
import qualified Data.List as L 
import System.Environment
import Control.Applicative


splitCombine i s = T.concat $ T.splitOn i s

splitOnIntron [] s = s
splitOnIntron (i:is) s = splitOnIntron is (splitCombine i s)

splitOnIntron' s = foldr splitCombine s

main = do
  path <- head <$> getArgs
  fa <- importFasta path
  let [s, introns] = [take 1 fa, tail fa]
  let spliced = splitOnIntron' (faseq $ head s) (map faseq introns)
  print spliced
  print $ (translate . T.unpack . dnaToRna) spliced

  
                        
