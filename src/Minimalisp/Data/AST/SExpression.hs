module Minimalisp.Data.AST.SExpression where

import Data.Number.CReal

data Noun
  = Number CReal
  | Character Char
  | Boolean Bool
  | Nil
  | Identifier String
  | NestedExpression SExpression
  deriving (Eq, Show)

data SExpression
  = FunctionCall String [Noun]
  | CaseExpression [(Noun, Noun)]
  | FunctionExpression [String] SExpression
  deriving (Eq, Show)
