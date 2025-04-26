module Miso.PrintableClass where

class Printable a where
  printItem :: a -> String
