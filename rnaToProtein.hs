{-#LANGUAGE OverloadedStrings#-}

module RnaToProtein
where

translate [] = []
translate rna = codon (take 3 rna) : translate (drop 3 rna)

codon r
  |take 2 r == "UU" = if lastUC r then 'F' else 'L'
  |take 2 r == "UC" = 'S'
  |take 2 r == "UA" = if lastUC r then 'Y' else 's'
  |take 2 r == "UG" = if lastUC r then 'C' else if last r == 'A' then 's' else 'W'
  |take 2 r == "CU" = 'L'
  |take 2 r == "CC" = 'P'
  |take 2 r == "CA" = if lastUC r then 'H' else 'Q'
  |take 2 r == "CG" = 'R'
  |take 2 r == "AU" = if lastUC r then 'I' else if last r == 'A' then 'I' else 'M'
  |take 2 r == "AC" = 'T'
  |take 2 r == "AA" = if lastUC r then 'N' else 'K'
  |take 2 r == "AG" = if lastUC r then 'S' else 'R'
  |take 2 r == "GU" = 'V'
  |take 2 r == "GC" = 'A'
  |take 2 r == "GA" = if lastUC r then 'D' else 'E'
  |take 2 r == "GG" = 'G'
  |otherwise = 'x'

lastUC r = last r == 'U' || last r == 'C'

-- IO
readRNA path = do
  rna <- readFile path >>= return . init 
  let protein = init $ translate rna
  writeFile "./outputRNA.txt" protein 
