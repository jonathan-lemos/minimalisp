module Minimalisp.Control.Parser.Text.CharEq where

import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.Conditional
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput
import Minimalisp.Control.Parser.Text.Char
import Minimalisp.Control.Parser.Text.Utils

charEq :: Char -> Parser Char
charEq c =
  conditional (== c) char \@/$ \ci ->
    "Expected " <> show c <> ", but got " <> diagnosticFirstChar ci <> "."

