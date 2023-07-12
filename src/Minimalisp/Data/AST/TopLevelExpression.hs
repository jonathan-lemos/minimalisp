module Minimalisp.Data.AST.TopLevelExpression where

import Minimalisp.Data.AST.SExpression

data TopLevelExpression = SExpr SExpression | DefineExpr String SExpression
