{-Problem

A matrix is a rectangular table of values divided into rows and columns. An m×n matrix has m rows and n columns. Given a matrix A, we write Ai,j to indicate the value found at the intersection of row i and column j.

Say that we have a collection of DNA strings, all having the same length n. Their profile matrix is a 4×n matrix P in which P1,j represents the number of times that 'A' occurs in the jth position of one of the strings, P2,j represents the number of times that C occurs in the jth position, and so on (see below).

A consensus string c is a string of length n formed from our collection by taking the most common symbol at each position; the jth symbol of c therefore corresponds to the symbol having the maximum value in the j-th column of the profile matrix. Of course, there may be more than one most common symbol, leading to multiple possible consensus strings.

A T C C A G C T
G G G C A A C T
A T G G A T C T
DNA Strings	A A G C A A C C
T T G G A A C T
A T G C C A T T
A T G G C A C T
A   5 1 0 0 5 5 0 0
Profile	C   0 0 1 4 2 0 6 1
G   1 1 6 3 0 1 0 0
T   1 5 0 0 0 1 1 6
Consensus	A T G C A A C T
Given: A collection of at most 10 DNA strings of equal length (at most 1 kbp) in FASTA format.

Return: A consensus string and profile matrix for the collection. (If several possible consensus strings exist, then you may return any one of them.)

Sample Dataset

>Rosalind_1
ATCCAGCT
>Rosalind_2
GGGCAACT
>Rosalind_3
ATGGATCT
>Rosalind_4
AAGCAACC
>Rosalind_5
TTGGAACT
>Rosalind_6
ATGCCATT
>Rosalind_7
ATGGCACT
Sample Output

ATGCAACT
A: 5 1 0 0 5 5 0 0
C: 0 0 1 4 2 0 6 1
G: 1 1 6 3 0 1 0 0
T: 1 5 0 0 0 1 1 6-}

import Data.List (foldl', intercalate)
import Data.List.Split (splitOn)

cols [xs] = [[x]|x<-xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

count [a, c, g, t] x
  |x=='A' = [a+1, c, g, t]
  |x=='T' = [a, c, g, t+1]
  |x=='G' = [a, c, g+1, t]
  |x=='C' = [a, c+1, g, t]
  |otherwise = [a, c, g, t]

concensus = foldl' count [0,0,0,0]

makeMatrix = cols . filter ((/= '>') . head) 

printATGC = map (map show) . cols 
printConcensus = map findConcensus 

createCountMatrix = map concensus . makeMatrix

addHead = map (intercalate " ") . zipWith (:) ["A:", "C:", "G:", "T:"]

findConcensus [a, c, g, t] 
  |and [a>=c, a>=g, a>=t] = 'A'
  |and [c>a, c>=g, c>=t] = 'C'
  |and [g>a, g>c, g>=t] = 'G'
  |and [t>a, t>c, t>g] = 'T'
  |otherwise = 'N'

--IO

-- run input twice, need to optimize the code
importSequence path = do
  input <- readFile path >>= return . createCountMatrix . map (filter (/='\n') . snd . break (=='\n')) . tail . splitOn ">" 
  let title = printConcensus input
  let atgc = unlines $ addHead $ printATGC input
  let output = title ++ "\n" ++ atgc
  writeFile "output.txt" output
