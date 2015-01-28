import Control.Applicative

inputInts::IO [Double]
inputInts = map read . words <$> getLine

odds = map (*2) [1, 1, 1, 0.75, 0.5, 0]

sol = zipWith (*) odds

main = do
  n <- inputInts
  print $ sum $ sol n
