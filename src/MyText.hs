{-#LANGUAGE OverloadedStrings#-}

{-
  routine functions for Data.Text manipulation.

  Min Zhang

  2/6/2015

-}

module MyText
where

import qualified Data.Text as T 
import qualified Data.Char as C
import Control.Applicative
import qualified Data.Map as M
import 

capitalize :: T.Text -> T.Text
capitalize x = T.cons (C.toUpper $ T.head x) (T.toLower $ T.tail x)

--Indexing
range :: (Int, Int) -> T.Text -> T.Text 
range (s, e) = T.take (e-s+1) . T.drop s

--read and show numbers
toInt :: T.Text -> Int
toInt = read . T.unpack

toDouble :: T.Text -> Double
toDouble = read . T.unpack

readInt :: Int -> T.Text
readInt = T.pack . show

readDouble :: Double -> T.Text
readDouble = T.pack . show 

-- 
chop :: T.Text -> T.Text
chop "" = ""
chop t
  |T.last t == '\n' = if T.last (T.init t) == '\r' then (T.init $ T.init t) else T.init t
  |otherwise = T.init t

chr :: T.Text -> T.Text
chr "" = ""
chr t = T.singleton $ T.head t

clear :: T.Text -> T.Text
clear _ = ""

isEmpty :: T.Text -> Bool
isEmpty t =
  case t of 
    "" -> True
    _  -> False

squeeze :: T.Text -> T.Text -> T.Text
squeeze t s = 
  if T.isInfixOf d s 
  then squeeze t (T.replace d t s)
  else s
  where d = T.replicate 2 t

-- separations
tab :: T.Text -> [T.Text]
tab = T.splitOn "\t"

untab :: [T.Text] -> T.Text
untab = T.intercalate "\t"

-- high order
eachLine :: (T.Text -> T.Text) -> T.Text -> T.Text
eachLine f = T.unlines . map f . T.lines

countElem f e = M.toList $ M.fromListWith (+) [(c, 1) | c <- input]
                 where input = filter f e
       

countChar t = countElem allTrue input
            where input = T.unpack t

countWords s = countElem allTrue input
            where input = T.words s

countLetter t = countElem C.isLetter input
            where input = T.unpack t

countSentence = undefined
countLines = undefined

allTrue _ = True

