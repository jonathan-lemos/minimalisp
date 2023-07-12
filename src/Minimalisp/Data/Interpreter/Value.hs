module Minimalisp.Data.AST.Value where

import Minimalisp.Data.AST.SExpression
  
data Value = Function [String] SExpression | Boolean Bool | Character Char | Nil
