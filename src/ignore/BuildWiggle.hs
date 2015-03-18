{-#LANGUAGE OverloadedStrings#-}
-- 12/11/14
-- Min Zhang
-- Martin lab
-- build empty wig file from fasta file

{-
  Wiggle format:

  General Structure

http://genome.ucsc.edu/goldenpath/help/wiggle.html

Wiggle format is line-oriented. For wiggle custom tracks, the first line must be a track definition line (i.e., track type=wiggle_0), which designates the track as a wiggle track and adds a number of options for controlling the default display.
Wiggle format is composed of declaration lines and data lines, and require a separate wiggle track definition line. There are two options for formatting wiggle data: variableStep and fixedStep. These formats were developed to allow the file to be written as compactly as possible.

variableStep is for data with irregular intervals between new data points and is the more commonly used wiggle format. After the wiggle track definition line, variableStep begins with a declaration line and is followed by two columns containing chromosome positions and data values:
  variableStep  chrom=chrN  [span=windowSize]
  chromStartA  dataValueA
  chromStartB  dataValueB
  ... etc ...  ... etc ...

Note: do not build the whole genome, instead build a portion of the genome (~1Mb around the view point)
-}

import System.Environment
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO

main = do
  putStrLn "\nBuildWiggle [start] [end] [outputFilePath]\n"
  [chromosome, start', end', outPath] <- getArgs
  let chr = chromosome
  let start = read start'::Int
  let end = read end'::Int
  let trackLine = T.pack "track type=wiggle_0"
  let varStepLine = T.pack $ ("variableStep\tchrom=" ++ chr ++ "\tspan=1\n")
  let output = T.concat [varStepLine, transformed start end]
  TextIO.writeFile outPath output

transformed start end = T.unlines $ map (\x-> T.pack $ show x ++ "\t0") [start..end]
