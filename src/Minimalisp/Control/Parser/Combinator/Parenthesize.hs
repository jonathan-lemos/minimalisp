module Minimalisp.Control.Parser.Combinator.Parenthesize where

import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.IgnoreWhitespace
import Minimalisp.Control.Parser.Text.CharEq

parenthesize :: Parser a -> Parser a
parenthesize p = do
  ignoreWhitespace $ charEq '('
  v <- p
  ignoreWhitespace $ charEq ')'
  return v

