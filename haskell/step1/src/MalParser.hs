module MalParser where

import           Control.Applicative (empty)
import           Control.Monad (void)
import           Text.Megaparsec
import           Text.Megaparsec.Combinator
import           Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc
