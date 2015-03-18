{-#LANGUAGE OverloadedStrings#-}
-- parse md tag and cigar tags

module ParseTags
       ( convertMdTag
       , convertCigarTag
       , mapMergeTags)
where

import qualified Data.Text as T
import qualified Data.List.Split as Sp

--select valid md tags
getValidMD :: [T.Text] -> [T.Text]
getValidMD = filter (\x->x/="noMd")

splitMD :: T.Text -> [T.Text]
splitMD = T.groupBy ifSameGroup
  where  ifSameGroup a b = ifNT a == ifNT b
         ifNT x = x `elem` "^ATGC"  

mdToNum :: T.Text -> T.Text
mdToNum ms | T.head ms == '^' = T.replicate (T.length ms - 1) "D"
           | T.head ms `elem` "ATGC" = ms
           | T.head ms `elem` "1234567890" = T.replicate (read $ T.unpack ms) "_"
           | otherwise = ms

--mdtag in julia, main function for md tags
convertMdTag :: [T.Text] -> [T.Text]
convertMdTag ms = map (T.concat . map mdToNum . splitMD) $ getValidMD ms

--parsecigar in julia
getValidCigar = filter (\x->x/="*")

splitCigar = sliceTwo . T.groupBy ifSameGroup
  where ifSameGroup a b = ifCigar a == ifCigar b
        ifCigar x = x `elem` "MIDNSHPX="  -- not covering everything in cigarToNum
        sliceTwo = Sp.chunksOf 2

--number and letter
cigarToNum [n,l] | l == "S" = ""
                 | l == "M" = T.replicate (toNumber n) "_"
                 | l == "D" = T.replicate (toNumber n) "D"
                 | l == "I" = "I" 
                 | otherwise = ""
                   where toNumber = read . T.unpack
cigarToNum _ = ""

joinCigar cs = T.intercalate "I" $ (\x->map T.init (init x) ++ [last x]) $ (T.splitOn "I") $ T.concat cs 

-- cigartag in julia
convertCigarTag :: [T.Text] -> [T.Text]
convertCigarTag cs = map (joinCigar . (map cigarToNum) . splitCigar) $ getValidCigar cs

mergeTags (cigar, md) = T.zipWith mergeRule cigar md
  where mergeRule x y | x == y = x
                      | x == '_' && y == 'I' = 'I'
                      | x == 'D' && y == '_' = 'D'
                      | y `elem` "ATGC" && x == '_' = y
                      | otherwise = y 
mapMergeTags cs ms = map mergeTags $ zip cs ms 

-- merge sequence of individual reads and tags, to show mutation and mapped sequence
--mergeSeqTag seq tags = 
--  breakTags = T.group tags 
--  seq = head' ++ tail'
--  map f breakTags 
--  foldl' f [] breakTags
--  f tag seq
--      | tag == "" = []
--      | T.head tag == "_" = (++) take (length tag) seq
--      | T.head tag == "D" = (++) tag
--      | otherwise = (++) tag
