module MalLexer where

import           Control.Applicative (empty)
import           Control.Monad (void)
import           Text.Megaparsec
import           Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

sc :: Parser ()
sc = L.space (void $ spaceChar <|> char ',') (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

malSymbol :: Parser String
malSymbol = lexeme $ some (noneOf (") " :: [Char]))
