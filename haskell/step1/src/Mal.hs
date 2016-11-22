module Mal where

import Text.Megaparsec

import MalLexer
import MalParser
import MalPrinter
import MalTypes

rep :: String -> String
rep = malPrint . myEval . myRead

myRead :: String -> Form
myRead s =
  case runParser (sc >> formParser) "" s of
    Left e  -> error $ "Parse error: " ++ parseErrorPretty e
    Right r -> r

myEval :: Form -> Form
myEval = id
