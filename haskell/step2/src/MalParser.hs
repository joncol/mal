{-# LANGUAGE OverloadedStrings #-}

module MalParser where

import qualified Data.Map as M
import           Text.Megaparsec
import           Text.Megaparsec.String

import           MalLexer
import           MalTypes

formParser :: Parser Form
formParser = choice [ listParser
                    , FVector <$> vectorParser
                    , FHashMap <$> hashMapParser
                    , FKeyword <$> keywordParser
                    , FAtom <$> atomParser
                    , quoteParser
                    , quasiquoteParser
                    , spliceUnquoteParser
                    , unquoteParser
                    , FMeta <$> metaDataParser
                    , FDeref <$> derefParser
                    ]

listParser :: Parser Form
listParser =  FList <$> (parens $ List <$> many formParser)

vectorParser :: Parser List
vectorParser = brackets $ List <$> many formParser

atomParser :: Parser Atom
atomParser = choice [ NumberAtom <$> integer
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

keywordParser :: Parser String
keywordParser = char ':' >> malSymbol

stringParser :: Parser MalString
stringParser = MalString <$> do
  _ <- char '\"'
  manyTill stringCharParser (char '\"') <* space

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

hashMapParser :: Parser HashMap
hashMapParser =
  braces $
  do
    xs <- many (keyValueParser1 <|> keyValueParser2)
    let m = M.fromList xs
    return $ HashMap m

keyValueParser1 :: Parser (Key, Form)
keyValueParser1 =
  do
    key   <- stringParser
    value <- formParser
    return (StringKey key, value)

keyValueParser2 :: Parser (Key, Form)
keyValueParser2 =
  do
    key   <- keywordParser
    value <- formParser
    return (KeywordKey key, value)

metaDataParser :: Parser (List, HashMap)
metaDataParser = do
  _ <- char '^'
  m <- hashMapParser
  l <- vectorParser
  return (l, m)

derefParser :: Parser Atom
derefParser = char '@' >> atomParser
