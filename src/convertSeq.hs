{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  1/25/2015

  combine all the converting functions into one.
-}

import System.Environment

import IO

main = do
  welcome
  getArgs >>= executeArgs

welcome = putStrLn "\nconvertSeq [Options] [Input/Output]\n"

executeArgs [a,b,c] = do
  case a of
    "sam2fq" -> samToFastq b c
    "sam2fa" -> samToFasta b c
    otherwise -> putStrLn "Not an option."
