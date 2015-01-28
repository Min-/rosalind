import Control.Applicative
import Control.Monad

main = do
  seq <- readSeq 
  k <- readInt
  putStrLn $ unlines $ replicateM k seq 

readInt :: IO Int
readInt = read <$> getLine

readSeq :: IO String
readSeq = filter (/= ' ') <$> getLine
