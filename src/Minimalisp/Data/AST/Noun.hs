module Minimalisp.Data.AST.Noun where

import Data.Number.CReal

data Noun = Number CReal | Character Char | Boolean Bool | Nil | Identifier String
  deriving (Eq, Show)
