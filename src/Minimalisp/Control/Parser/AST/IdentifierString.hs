module Minimalisp.Control.Parser.AST.IdentifierString where

import Control.Applicative
import Data.Char
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.Conditional
import Minimalisp.Control.Parser.Text.Char
import Minimalisp.Control.Parser.Text.StringWhile
import Minimalisp.Control.Parser.Combinator.IgnoreWhitespace

identifierString :: Parser String
identifierString = ignoreWhitespace $ do
  first <- conditional isAlpha char
  remainder <- stringWhile (liftA2 (||) isAlphaNum (`elem` "-_"))
  return $ first : remainder
