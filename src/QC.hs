{-#LANGUAGE OverloadedStrings#-}

-- functions for work flow 

module QC
where


import qualified Data.Text as T

import Util

-- Function1 check if restriction enzyme sites are existed, which one? or both?

hindIII = "AAGCTT"::T.Text
dpnII = "GATC"::T.Text

-- s: sequence; e: enzyme
isEnzymeThere e s = T.isInfixOf e s

-- how many reads contain enzyme; both enzymes, f = and; either enzyme; f = or
-- what's the pct of the reads contain enzyme
-- combine with utility function groupCount or groupPct
checkEnzyme f = map (\x->T.pack $ show $ f [isEnzymeThere dpnII x, isEnzymeThere hindIII x])

-- how many reads contain correct primer sequences?
-- either primers or both primers?
-- how to handle small mismatches in the primers?



