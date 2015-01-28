-- m = 3

--young n = old (n-1)
--
--old 1 = 0
--old 2 = 1
--old 3 = 1
--old n = old (n-2) + old (n-3)
--
--total n = young n + old n

import Data.List (foldl')

gOld 0 _ = 0::Integer
gOld 1 _ = 0
gOld 2 _ = 1
gOld n m
   | m == n - 1 = gOld (n-1) m
   | m < n  = gOld (n-2) m + gOld (n-1) m - gOld (n-m-1) m
   | otherwise = 2^(n-3) -- great!!
--  | m < n = sum' $ map (\x-> gOld x m) [n-m .. n-2] 
--  | otherwise = sum' $ map (\x-> gOld x m) [0 .. n-1]

gYoung n m = gOld (n-1) m
gTotal n m = gYoung n m + gOld n m 


-- some efforts to keep all the values:
{---  | --  | m < n = let prefix = map (\x->gOld x m) [1..m+1] 
--            in last $ foldl' (acc m) prefix [1..m-n]

acc m p [] = p
acc m p x:xs = p ++ (acc m updatep xs) 
               where updatep = last p + (head $ drop 1 $ (reverse p)) - p!!(length p - m -1)

sum [] = 0
sum x:xs = x+sum xs

-}

createWindow m = 0:1:rest ++ [last' + sndlast - 1]
                 where rest = map old [3..m]
                       old = fib' -- fast fib'
                       last' = last rest
                       sndlast = head $ drop 1 $ reverse rest
-- fib is naive implentation, but still good enough
fib 0 = 1 -- first young
fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

fib' 1 = 0
fib' 2 = 1
fib' n = last $ foldl' shiftWindow [0, 1] [3..n]
       where shiftWindow w x = tail w ++ [head w + last w] 

                    -- old x = 2^(x-3) -- works on m = 4; but doesn't work on m = 5; should use fib
-- This is awesome!! 
total n m = old + young
           where old = last lastWindow
                 young = head $ drop 1 $ reverse lastWindow
                 lastWindow = foldl' (manageWindow ref) window [1..n-m-1]           
                 window = createWindow m
                 ref = refWindow m

manageWindow r w x = tail w ++ [sum $ zipWith (*) r w]

refWindow m = concat [[-1], replicate (m-2) 0, [1], [1]]
