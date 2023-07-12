module Minimalisp.Control.Parser.Text.StringWhile where

import Minimalisp.Control.Parser
import Control.Applicative
import Minimalisp.Control.Parser.Combinator.Conditional
import Minimalisp.Control.Parser.Text.Char

stringWhile :: (Char -> Bool) -> Parser String
stringWhile predicate = many (conditional predicate char)
