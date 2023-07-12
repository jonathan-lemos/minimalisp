module Minimalisp.Control.Parser.Text.Whitespace where

import Data.Char
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Text.StringWhile

whitespace :: Parser String
whitespace = stringWhile isSpace
