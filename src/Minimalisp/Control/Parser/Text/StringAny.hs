module Minimalisp.Control.Parser.Text.StringAny where

import Control.Applicative
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput
import Minimalisp.Control.Parser.Text.StringEq
import Minimalisp.Control.Parser.Text.Utils

stringAny :: [String] -> Parser String
stringAny ss =
  asum (stringEq <$> ss) \@/$ \ci ->
    "Expected any of " <> show ss <> ", but got " <> diagnosticToken ci
