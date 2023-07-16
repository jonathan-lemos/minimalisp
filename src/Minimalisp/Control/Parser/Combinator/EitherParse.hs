module Minimalisp.Control.Parser.Combinator.EitherParse where

import Minimalisp.Control.Parser
import Minimalisp.Data.ParseError

eitherParse :: Parser a -> Parser (Either ParseError a)
eitherParse p = Parser $ \s ->
  case parse p s of
    Left pe -> Right (s, Left pe)
    Right (s2, v) -> Right (s2, Right v)
