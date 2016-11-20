module Mal where

import System.Console.Haskeline

rep :: String -> String
rep = myPrint . myEval . myRead

myRead :: String -> String
myRead = id

myEval :: String -> String
myEval = id

myPrint :: String -> String
myPrint = id
