module Minimalisp.Control.Parser.Combinator.ResetErrorPosition where

import Data.Bifunctor
import Minimalisp.Control.Parser
import Minimalisp.Data.ParseError

resetErrorPosition :: Parser a -> Parser a
resetErrorPosition p = Parser $ \s ->
  first (\e -> e {currentInput = s}) $ parse p s
