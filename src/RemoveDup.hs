{-#LANGUAGE OverloadedStrings#-}

-- remove duplicate reads from Forward and reverse primer matching based on their read name

-- use primer set 2 as an example
-- forward 

import IO
import DataTypes
import System.Environment
import qualified Data.List as L
import qualified Data.Text as T

main = do
  [p1, p2, output] <- getArgs
  f1 <- fmap selectLongerReads $ importFasta p1
  f2 <- fmap selectLongerReads $ importFasta p2
  putStrLn $ "The sample1 has " ++ (show $ length f1) ++ "lines."
  putStrLn $ "The sample2 has " ++ (show $ length f2) ++ "lines."
  let combinedUnique = map head $ L.group $ concat [f1, f2] 
  putStrLn $ "The combined sample has " ++ (show $ length combinedUnique) ++ "lines."
  outputFasta output combinedUnique

selectLongerReads fas = filter (\fa->T.length (faseq fa) >= 20) fas

