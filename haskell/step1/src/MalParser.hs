{-# LANGUAGE OverloadedStrings #-}

module MalParser where

import Text.Megaparsec
-- import Text.Megaparsec.Combinator
import Text.Megaparsec.String

import MalLexer
import MalTypes

formParser :: Parser Form
formParser = FList <$> listParser <|> FAtom <$> atomParser

listParser :: Parser List
listParser = parens $ List <$> many formParser

atomParser :: Parser Atom
atomParser = Number <$> integer <|> Symbol <$> malSymbol
