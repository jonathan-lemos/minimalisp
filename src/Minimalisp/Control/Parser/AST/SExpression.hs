module Minimalisp.Control.Parser.AST.SExpression where

import Control.Applicative
import Data.Char
import Data.List (singleton)
import Data.Number.CReal
import Minimalisp.Control.Parser
import Minimalisp.Control.Parser.AST.IdentifierString
import Minimalisp.Control.Parser.Combinator.Conditional
import Minimalisp.Control.Parser.Combinator.IgnoreWhitespace
import Minimalisp.Control.Parser.Combinator.MapErrorReasonFromCurrentInput
import Minimalisp.Control.Parser.Combinator.Parenthesize
import Minimalisp.Control.Parser.Combinator.WithErrorReason
import Minimalisp.Control.Parser.Text.Char
import Minimalisp.Control.Parser.Text.CharAny
import Minimalisp.Control.Parser.Text.CharEq
import Minimalisp.Control.Parser.Text.FoldChars
import Minimalisp.Control.Parser.Text.StringAny
import Minimalisp.Control.Parser.Text.StringEq
import Minimalisp.Control.Parser.Text.Utils
import Minimalisp.Data.AST.SExpression
import Text.Read (readMaybe)

number :: Parser Noun
number = do
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

character :: Parser Noun
character =
  let folder revString (Just newChar) =
        case (readMaybe (reverse revString) :: Maybe Char) of
          Just c -> Right (Right c)
          Nothing -> Right (Left (newChar : revString))
      folder _ Nothing = Left "Reached EOF before reading closing \'"
   in Character <$> foldChars folder "" \@/ "Expected a character literal."

boolean :: Parser Noun
boolean = Boolean . read <$> stringAny ["true", "false"]

nil :: Parser Noun
nil = Nil <$ stringEq "nil"

identifier :: Parser Noun
identifier = Identifier <$> identifierString

nestedExpr :: Parser Noun
nestedExpr = NestedExpression <$> sExpression

noun :: Parser Noun
noun =
  asum [number, character, boolean, nil, identifier, nestedExpr]
    \@/$ \ci ->
      "Expected a value or parentheses, got "
        <> diagnosticToken ci

functionInner :: Parser SExpression
functionInner =
  liftA3
    (const FunctionExpression)
    (ignoreWhitespace $ stringEq "function")
    (parenthesize $ many identifierString)
    sExpression

caseTerm :: Parser (Noun, Noun)
caseTerm = liftA2 (,) noun noun

caseInner :: Parser SExpression
caseInner =
  parenthesize $
    ignoreWhitespace (stringEq "case")
      >> CaseExpression <$> (some caseTerm \@/ "A case expression requires at least one condition.")

functionCallInner :: Parser SExpression
functionCallInner =
  parenthesize $ liftA2 FunctionCall identifierString (many noun)

sExpression :: Parser SExpression
sExpression =
  parenthesize $
    functionInner <|> caseInner <|> functionCallInner \@/ "Expected a function declaration, function call, or case expression"
