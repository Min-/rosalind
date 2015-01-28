import Control.Applicative
import Data.List (foldl')

mass [] = 0
mass (x:xs) = match x + mass xs

match x
    |x=='A' =  71.03711
    |x=='C' =  103.00919
    |x=='D' =  115.02694
    |x=='E' =  129.04259
    |x=='F' =  147.06841
    |x=='G' =  57.02146
    |x=='H' =  137.05891
    |x=='I' =  113.08406
    |x=='K' =  128.09496
    |x=='L' =  113.08406
    |x=='M' =  131.04049
    |x=='N' =  114.04293
    |x=='P' =  97.05276
    |x=='Q' =  128.05858
    |x=='R' =  156.10111
    |x=='S' =  87.03203
    |x=='T' =  101.04768
    |x=='V' =  99.06841
    |x=='W' =  186.07931
    |x=='Y' =  163.06333 
    |otherwise =  0

main = do
  mass <$> getLine


