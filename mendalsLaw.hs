{-
Given: Three positive integers k, m, and n, representing a population containing k+m+n organisms: k individuals are homozygous dominant for a factor, m are heterozygous, and n are homozygous recessive.

Return: The probability that two randomly selected mating organisms will produce an individual possessing a dominant allele (and thus displaying the dominant phenotype). Assume that any two organisms can mate.

Sample Dataset

2 2 2
Sample Output

0.78333
-}

dom = [True, True]
het = [True, False]
rec = [False, False]

-- possessing one dominant allele: 
-- True || True = True
-- True || False = True
-- False || False = False

-- get True if choose from dom 1
-- get True if choose from het 1/2

-- dom X het = 1
-- need do combination first
cp [] = [[]]
cp [x, y] = [[a,b] | a<-x, b<-y] 

checkDom = map or . cp 

domXdom = checkDom [dom, dom] --1
hetXhet = checkDom [het, het] --3/4
recXrec = checkDom [rec, rec] --0
domXhet = checkDom [dom, het] --1
domXrec = checkDom [dom, rec] --1
hetXrec = checkDom [het, rec] --1/2

-- next step is to create the binary tree
-- what's the possibility that generate True, given k m and n
-- total = k + m + n (WRONG!!) -- total is not trivial

-- get two from total (WRONG! need to write down on paper!!)
-- 1: 2 * k/total
-- 2: 2 * m/total
-- 3: 2 * n/total
-- 4: (k+m)/total
-- 5: (k+n)/total
-- 6: (m+n)/total

-- to get True:
{-
  combination k 2/total*1 + combination m 2/total*3/4 + 0 + (k*m)/total*1 + (k*n)/total*1 + (m*n)/total*1/2
-}

testPobability k m n = combination k 2/t*1 + combination m 2/t*3/4 + 0 + (k*m)/t*1 + (k*n)/t*1 + (m*n)/t*1/2
                       where t = total k m n 

total k m n = combination k 2 + combination m 2 + combination n 2 + k*m + k*n + m*n

product' n = foldl (*) 1 seq
             where seq = [1..n]

combination n k
  |n < k = 0
  |otherwise = (product' n) / ((product' k) * (product' (n-k)))

-- the solution doesn't consider probability tree yet!
