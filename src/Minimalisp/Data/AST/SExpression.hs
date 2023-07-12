module Minimalisp.Data.AST.SExpression where

import Minimalisp.Data.AST.Noun
import Minimalisp.Data.AST.Verb

data SExpression = SExpression Verb [Noun]
  deriving (Eq, Show)
