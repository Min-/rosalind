import Data.List
permutation n = permutations $ map show [1..n]

bind = unlines . map (intercalate " ")
