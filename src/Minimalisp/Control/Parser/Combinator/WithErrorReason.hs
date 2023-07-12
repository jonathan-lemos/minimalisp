module Minimalisp.Control.Parser.Combinator.WithErrorReason where

import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput
import Minimalisp.Control.Parser

withErrorReason :: String -> Parser a -> Parser a
withErrorReason = mapErrorReasonFromCurrentInput . const

(\@/) :: Parser a -> String -> Parser a
(\@/) = flip withErrorReason

infixl 2 \@/
