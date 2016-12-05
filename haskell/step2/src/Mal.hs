module Mal where

import Text.Megaparsec

import MalLexer
import MalParser
import MalPrinter
import MalTypes

rep :: String -> Either (ParseError Char Dec) String
rep s = (myEval . myRead) s >>= return . malPrint

myRead :: String -> Either (ParseError Char Dec) Form
myRead s = runParser (sc >> formParser) "" s

myEval :: m Form -> m Form
myEval = id
