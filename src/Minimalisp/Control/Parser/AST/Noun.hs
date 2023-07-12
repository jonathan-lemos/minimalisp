module Minimalisp.Control.Parser.AST.Noun (noun) where

import Control.Applicative
import Data.Char
import Data.List (singleton)
import Data.Number.CReal
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.Combinator.Conditional
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput
import Minimalisp.Control.Parser.Combinator.WithErrorReason
import Minimalisp.Control.Parser.Text.Char
import Minimalisp.Control.Parser.Text.CharAny
import Minimalisp.Control.Parser.Text.CharEq
import Minimalisp.Control.Parser.Text.FoldChars
import Minimalisp.Control.Parser.Text.StringAny
import Minimalisp.Control.Parser.Text.StringEq
import Minimalisp.Control.Parser.Text.StringWhile
import Minimalisp.Control.Parser.Text.Utils
import Minimalisp.Data.AST.Noun
import Text.Read (readMaybe)

_number :: Parser Noun
_number = do
  sign <- singleton <$> charAny "+-" <|> mempty

  ns <-
    some (conditional isDigit char)
      \@/$ \ci -> "Expected digits, got " <> diagnosticFirstChar ci

  decimalPart <-
    ( do
        charEq '.'
        some (conditional isDigit char)
      )
      <|> mempty

  expPart <-
    ( do
        charAny "eE"
        some (conditional isDigit char)
      )
      <|> mempty

  return $ Number (read (concat [sign, ns, decimalPart, expPart]) :: CReal)

_character :: Parser Noun
_character =
  let folder revString newChar =
        case (readMaybe (reverse revString) :: Maybe Char) of
          Just c -> Right c
          Nothing -> Left (newChar : revString)
   in Character <$> foldChars folder "" \@/ "Expected a character literal."

_boolean :: Parser Noun
_boolean = Boolean . read <$> stringAny ["true", "false"]

_nil :: Parser Noun
_nil = Nil <$ stringEq "nil"

_identifier :: Parser Noun
_identifier = do
  first <- conditional isAlpha char
  remainder <- stringWhile (liftA2 (||) isAlphaNum (`elem` "-_"))
  return $ Identifier (first : remainder)

noun :: Parser Noun
noun =
  asum [_number, _character, _boolean, _nil, _identifier]
    \@/$ \ci ->
      "Expected a number, character, boolean, \"nil\" or a function name, got "
        <> diagnosticToken ci
