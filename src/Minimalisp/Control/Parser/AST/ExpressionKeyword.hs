module Minimalisp.Control.Parser.AST.ExpressionKeyword where

import Control.Applicative
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput
import Minimalisp.Control.Parser.Text.StringEq
import Minimalisp.Control.Parser.Text.Utils
import Minimalisp.Data.AST.ExpressionKeyword
import Minimalisp.Control.Parser.Combinator.IgnoreWhitespace

expressionKeyword :: Parser ExpressionKeyword
expressionKeyword =
  ignoreWhitespace (Case <$ stringEq "case" <|> Function <$ stringEq "function")
    \@/$ \ci -> "Expected \"case\" or \"function\", got " <> diagnosticToken ci <> "."
