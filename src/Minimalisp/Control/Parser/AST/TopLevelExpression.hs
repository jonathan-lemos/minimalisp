module Minimalisp.Control.Parser.AST.TopLevelExpression where

import Minimalisp.Data.AST.TopLevelExpression
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.Parenthesize
import Control.Applicative
import Minimalisp.Control.Parser.Text.StringEq
import Minimalisp.Control.Parser.Combinator.IgnoreWhitespace
import Minimalisp.Control.Parser.AST.IdentifierString
import Minimalisp.Control.Parser.AST.SExpression

defineExpr :: Parser TopLevelExpression
defineExpr = parenthesize $
  liftA3
    (const DefineExpr)
      (ignoreWhitespace $ stringEq "define")
      identifierString
      sExpression

topLevelExpression :: Parser TopLevelExpression
topLevelExpression = defineExpr <|> SExpr <$> sExpression
