module Minimalisp.Control.Parser.Text.FoldChars where

import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Text.Char

foldChars :: (a -> Char -> Either a b) -> a -> Parser b
foldChars f state = do
  c <- char
  case f state c of
    Left newState -> foldChars f newState
    Right v -> pure v
