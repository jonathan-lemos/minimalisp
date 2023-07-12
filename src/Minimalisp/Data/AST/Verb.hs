module Minimalisp.Data.AST.Verb where

import Minimalisp.Data.AST.ExpressionKeyword

data Verb = SpecialForm ExpressionKeyword | FunctionCall String
  deriving (Eq, Show)
