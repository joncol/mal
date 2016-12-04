{-# LANGUAGE OverloadedStrings #-}

module MalParser where

import Text.Megaparsec
import Text.Megaparsec.String

import MalLexer
import MalTypes

formParser :: Parser Form
formParser = FList <$> listParser <|> FAtom <$> atomParser

listParser :: Parser List
listParser = (parens $ List <$> many formParser) <?> "list"

atomParser :: Parser Atom
atomParser = NumberAtom <$> integer <|>
             StringAtom <$> stringParser <|>
             SymbolAtom <$> malSymbol

stringCharParser :: Parser Char
stringCharParser =
  do
    c <- anyChar
    case c of
      '\\' -> do
        x <- anyChar
        case x of
          '\"' -> return '\"'
          'n'  -> return '\n'
          '\\' -> return '\\'
          _    -> fail $ "Unexpected character " ++ [x]
      _ -> return c

stringParser :: Parser MalString
stringParser = MalString <$> do
  _ <- char '\"'
  manyTill stringCharParser (char '\"')
