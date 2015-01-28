import Data.List (foldl', sortBy)
import Data.List.Split (splitOn)

gc = foldl' count (0, 0)
  
count (l, g) x = 
  if x == 'G' || x == 'C'
  then (l+1, g+1)
  else if x == '\n'
       then (l, g)
       else (l+1, g)

gcPc (l, g) = fromIntegral g / fromIntegral l * 100

--IO
importFasta path = do
  fa <- readFile path
  let result = head $ sortBy gcPcCompare $ map (countGC . break (=='\n')) $ tail $ splitOn ">" fa
  print result 

countGC (a, b) = (a, gcPc $ gc b)

gcPcCompare (a1, b1) (a2, b2)
  | b1 < b2 = GT
  | b1 > b2 = LT
  | b1 == b2 = compare a1 a2
