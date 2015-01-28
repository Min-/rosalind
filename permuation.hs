{-
Problem

A permutation of length n is an ordering of the positive integers {1,2,…,n}. For example, π=(5,3,2,1,4) is a permutation of length 5.

Given: A positive integer n≤7.

Return: The total number of permutations of length n, followed by a list of all such permutations (in any order).
-}
import qualified Data.List as L

nInt :: Int -> [Int]
nInt n = take n [1..7]

cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- xss] 

permutation = permutations
