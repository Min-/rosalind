{-#LANGUAGE OverloadedStrings#-}

{-
  1/21/2015
  Min Zhang

  hSamTools/hst
  Executable for general Sam files manipulation.

  The function format is:

  hst options input output

  options use flags: -

  -- comment (1/25/15) probably still need to stick with one function at a time.

-}

import IO
import DataTypes
import qualified Data.Text as T

import System.Environment (getArgs)
import Control.Applicative
import Control.Monad
import Safe
import Data.Maybe

data Args = Args
     {task :: Maybe String
     ,method :: Maybe String
     ,input :: Maybe FilePath
     ,output :: Maybe FilePath
     ,parameters :: Maybe String
     } deriving (Eq, Show, Read)

main = do
  welcome >> parseArgs >>= executeCmd >> reportResults

---
welcome = do
  putStrLn "hts -t [task] -m [methods] -i [input] -o [output]"

parseArgs = getArgs >>= return . sortArgs . pairArgs

executeCmd = runArgs

reportResults = do
  putStrLn "Everything is fine."

--- parseArgs

pairArgs x = zip flags nonflags
             where flags = filter ((==) '-' . head) x
                   nonflags = filter ((/=) '-' . head) x

sortArgs l = Args (getOptions "-t" l) (getOptions "-m" l) (getOptions "-i" l) (getOptions "-o" l) (getOptions "-p" l) 
             where getOptions o = fmap snd . headMay . filter ((==) o . fst)

--- executeCmd 
runArgs a = do
  case task a of
    Nothing -> print "No task was assigned."
    Just "Convert" -> runConvert a
    Just "Count" -> runCount a
    Just "Trim" -> runTrim a
    otherwise -> print "Check if the task was input correctly (case sensitive). [Convert | Count | trim]."                                

runConvert a = do
  case m of
    Nothing -> print "No method assigned for Convert task."
    Just "samtofasta" -> samToFasta i o
    Just "samtofastq" -> samToFastq i o
    otherwise -> print "Check the options for Convert (case sensitive): [samtofasta | samtofastq]. " 
  where i = (fromJust . input) a
        o = (fromJust . output) a
        m = method a

runCount a = do
  case m of
    Nothing -> print "No method assigned for Count task."
    Just "all" -> countOptions i "all" p
    Just "filter" -> countOptions i "filter" p
  where i = (fromJust . input) a
        o = (fromJust . output) a
        m = method a
        p = (parseParameters . parameters) a

countOptions i option p = do
  case option of 
    "all" -> length' file
    "filter" -> length' $ filter p file
  where file = case (reverse . take 4 . reverse) i of
                 ".sam" -> importSamFile i
                 "asta" -> importFasta i
                 otherwise -> []
               
parseParameters q = 
  case q of 
    Nothing -> id
    Just s -> s

runTrim a = undefined

