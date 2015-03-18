{-# LANGUAGE DeriveDataTypeable #-}

{-
  IO deal with input arguments.
  You should be convinced to use this system.
-}

module CmdArgsSample
where

import System.Console.CmdArgs.Implicit
import Data.Data

data Args = Args { mode :: String
                 , detail :: String
                 , input :: FilePath
                 , output :: FilePath
                 } deriving (Show, Data, Typeable)

arguments = Args{  mode = def -- default value for String ""
                     &= help "Modes: convert, count, trim"
                 , detail = def
                     &= help "detailed commands"
                 , input = def
                     &= typFile
                     &= help "Input file path."
                 , output = def
                     &= typFile
                     &= help "Output file path."
                 }
         &= summary "hSamtools: Sam files manipulation. v0.0.1, (C) Min Zhang 2015"
         &= help "hSamtools -m=[options/modes] -i=[input] -o=[output]"
         &= program "hSamtools"

