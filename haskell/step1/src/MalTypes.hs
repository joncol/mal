module MalTypes where

data Form = FList List | FAtom Atom deriving (Eq, Show)
newtype List = List [Form] deriving (Eq, Show)
data Atom = Number Integer | Symbol String deriving (Eq, Show)
