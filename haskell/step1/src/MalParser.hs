{-# LANGUAGE OverloadedStrings #-}

module MalParser where

import Text.Megaparsec
import Text.Megaparsec.String

import MalLexer
import MalTypes

formParser :: Parser Form
formParser = choice [ listParser
                    , vectorParser
                    , atomParser
                    , quoteParser
                    , quasiquoteParser
                    , spliceUnquoteParser
                    , unquoteParser
                    ]

listParser :: Parser Form
listParser =  FList <$> (parens $ List <$> many formParser)

vectorParser :: Parser Form
vectorParser =  FVector <$> (brackets $ List <$> many formParser)

atomParser :: Parser Form
atomParser = FAtom <$> choice [ NumberAtom <$> integer
                              , StringAtom <$> stringParser
                              , SymbolAtom <$> malSymbol
                              ]

quoteParser :: Parser Form
quoteParser = FQuote <$> (char '\'' >> formParser)

quasiquoteParser :: Parser Form
quasiquoteParser = FQuasiquote <$> (char '`' >> formParser)

unquoteParser :: Parser Form
unquoteParser = FUnquote <$> (char '~' >> formParser)

spliceUnquoteParser :: Parser Form
spliceUnquoteParser = FSpliceUnquote <$> (string "~@" >> formParser)

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
