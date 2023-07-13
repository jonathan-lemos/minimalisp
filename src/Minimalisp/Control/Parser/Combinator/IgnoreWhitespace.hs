module Minimalisp.Control.Parser.Combinator.IgnoreWhitespace where

import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Text.Whitespace

ignoreWhitespace :: Parser a -> Parser a
ignoreWhitespace = (whitespace >>)
