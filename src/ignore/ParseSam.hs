{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  1/16/15

  rewrite sam file parser, to get more efficiency and more information
-}

import Data.Attoparsec.Text
import qualified Data.Text as T

data Reads = Reads
  {  qname	::T.Text
   , flag	::Int
   , rname	::T.Text
   , pos	::Int
   , mapq	::Int
   , cigar	::T.Text
   , rnext	::T.Text
   , pnext	::T.Text
   , tlen	::Int
   , samseq	::T.Text
   , qual	::T.Text
   , md		::T.Text
  } deriving (Show, Read, Eq)

data One = One {a::T.Text}

samParser = do
  skipWhile (=='@')
  q <- anyChar
  space  
  return q
  

