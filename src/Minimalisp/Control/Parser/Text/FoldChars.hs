module Minimalisp.Control.Parser.Text.FoldChars where

import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.MaybeParse
import Minimalisp.Control.Parser.Text.Char
import Minimalisp.Data.ParseError

foldChars :: (a -> Maybe Char -> Either String (Either a b)) -> a -> Parser b
foldChars f state = Parser $ \s -> do
  (s2, c) <- parse (maybeParse char) s
  case f state c of
    Left err -> Left $ ParseError {reason = err, currentInput = s}
    Right (Left newState) -> parse (foldChars f newState) s2
    Right (Right v) -> Right (s2, v)
