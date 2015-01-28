import Control.Applicative
import Control.Monad
import Text.Printf

inputDoubles :: IO [Double]
inputDoubles = map read . words <$> getLine

calOdds [] gc = 1::Double
calOdds (x:xs) gc 
  | x=='G' || x=='C'= gc/2 * calOdds xs gc
  | otherwise = (1- gc)/2 * calOdds xs gc  

sol dna gc = map (logBase 10 . calOdds dna) gc 

main = do
  dna <- getLine
  gcPct <- inputDoubles
  let output = sol dna gcPct
  mapM_ (\x->printf "%.3f " x)  output
         


