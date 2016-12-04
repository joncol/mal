module MalTypes where

import           Data.List (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           MalPrinter

data Form = FList List
          | FVector List
          | FHashMap HashMap
          | FAtom Atom
          | FQuote Form
          | FQuasiquote Form
          | FUnquote Form
          | FSpliceUnquote Form
          | FKeyword String
          deriving (Eq, Show)

newtype List = List [Form] deriving (Eq, Show)

newtype HashMap = HashMap (Map Key Form) deriving (Eq, Show)

data Key = StringKey MalString | KeywordKey String deriving (Eq, Ord, Show)

instance Printable Key where
  malPrint (StringKey s) = malPrint s
  malPrint (KeywordKey k) = ":" ++ k

data Atom = NumberAtom Integer
          | StringAtom MalString
          | SymbolAtom String
          deriving (Eq, Show)

newtype MalString = MalString String deriving (Eq, Ord, Show)

instance Printable Form where
  malPrint (FList l)          = "(" ++ malPrint l ++ ")"
  malPrint (FVector l)        = "[" ++ malPrint l ++ "]"
  malPrint (FHashMap m)       = "{" ++ malPrint m ++ "}"
  malPrint (FAtom a)          = malPrint a
  malPrint (FQuote q)         = "(quote " ++ malPrint q ++ ")"
  malPrint (FQuasiquote q)    = "(quasiquote " ++ malPrint q ++ ")"
  malPrint (FUnquote q)       = "(unquote " ++ malPrint q ++ ")"
  malPrint (FSpliceUnquote q) = "(splice-unquote " ++ malPrint q ++ ")"
  malPrint (FKeyword s)       = ":" ++ s

instance Printable List where
  malPrint (List fs) = intercalate " " $ map malPrint fs

instance Printable HashMap where
  malPrint (HashMap m) = intercalate " " $ map pkv $ M.toList m
    where pkv (k, v) = malPrint k ++ " " ++ malPrint v

instance Printable Atom where
  malPrint (NumberAtom n) = show n
  malPrint (StringAtom s) = malPrint s
  malPrint (SymbolAtom s) = s

instance Printable MalString where
  malPrint (MalString s) = show s
