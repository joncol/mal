module MalTypes where

import Data.List (intercalate)

import MalPrinter

data Form = FList List | FAtom Atom deriving (Eq, Show)
newtype List = List [Form] deriving (Eq, Show)

data Atom = NumberAtom Integer
          | StringAtom MalString
          | SymbolAtom String
          deriving (Eq, Show)

newtype MalString = MalString String deriving (Eq, Show)

instance Printable Form where
  malPrint (FList l) = malPrint l
  malPrint (FAtom a) = malPrint a

instance Printable List where
  malPrint (List fs) = "(" ++ (intercalate " " ss) ++ ")"
    where ss = map malPrint fs

instance Printable Atom where
  malPrint (NumberAtom n) = show n
  malPrint (StringAtom s) = malPrint s
  malPrint (SymbolAtom s) = s

instance Printable MalString where
  malPrint (MalString s) = show s
