module Minimalisp.Control.Parser.Text.CharAny where

import Control.Applicative
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Text.CharEq
import Minimalisp.Control.Parser.Text.Utils
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput

charAny :: String -> Parser Char
charAny s =
  asum (charEq <$> s)
    \@/$ (\ci -> "Expected any of " <> show s <> ", but got " <> diagnosticFirstChar ci)
