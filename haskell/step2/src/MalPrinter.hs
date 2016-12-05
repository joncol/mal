module MalPrinter where

class Printable a where
  malPrint :: a -> String
