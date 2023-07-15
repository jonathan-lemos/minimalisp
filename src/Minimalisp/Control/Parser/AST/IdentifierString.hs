module Minimalisp.Control.Parser.AST.IdentifierString where

import Control.Applicative
import Data.Char
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.Conditional
import Minimalisp.Control.Parser.Text.Char
import Minimalisp.Control.Parser.Text.StringWhile
import Minimalisp.Control.Parser.Combinator.IgnoreWhitespace
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput
import Minimalisp.Control.Parser.Text.Utils

identifierString :: Parser String
identifierString = ignoreWhitespace $ do
  first <- conditional isAlpha char \@/$ \ci -> "Expected a-zA-Z, but got " <> diagnosticFirstChar ci
  remainder <- stringWhile (liftA2 (||) isAlphaNum (`elem` "-_"))
  return $ first : remainder
