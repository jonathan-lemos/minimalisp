module Minimalisp.Control.Parser.Text.Char where

import Minimalisp.Control.Parser
import Minimalisp.Data.ParseError

char :: Parser Char
char = Parser $ \s ->
  case s of
    (x : xs) -> Right (xs, x)
    [] ->
      Left $
        ParseError
          { reason = "Expected any character, but got EOF.",
            currentInput = s
          }
