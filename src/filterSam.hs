{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  01/09/15

  after bowtie2 mapping filter out the mapped reads with exact 1 match
  bowtie2 mapping parameters:
  
  bowtie2 -f (fasta input) --fast (end-to-end mode) -x (mm9) -S

  for no matching reads check flag == 4
  for multiple matching check if the line contains (XS:i:) then multiple alignment, remove it

  however, in DataTypes module, the XS:i field was trimmed before examining.
-}

import IO
import DataTypes

import qualified Data.Text as T
import qualified Data.Text.IO as TextIO

-- copy importSamFile from IO, modified it, remove lines contain XS:i: before import as [Reads]

removeXS = filter (\x-> not $ T.isInfixOf "XS:i" x)

main = do
  putStrLn "Enter input Sam file path: "
  samPath <- getLine
  putStrLn "Enter output Sam file Path: "
  outputPath <- getLine
  TextIO.readFile samPath >>= TextIO.writeFile outputPath . T.unlines . removeXS . T.lines


