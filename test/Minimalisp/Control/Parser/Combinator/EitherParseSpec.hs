module Minimalisp.Control.Parser.Combinator.EitherParseSpec where

import Data.Either
import Harness.ParserCase
import Harness.QuickCheckParser
import Minimalisp.Control.Parser.Combinator.EitherParse
import Minimalisp.Control.Parser.Text.CharEq
import Minimalisp.Control.Parser.Text.StringEq
import Minimalisp.Data.ParseError
import Test.Hspec

spec :: Spec
spec = do
  parserCase
    (eitherParse $ stringEq "ab")
    "eitherParse $ stringEq \"ab\""
    $ do
      "ab" `shouldParseTo` Right "ab" `withRemainder` ""
      "abc" `shouldParseTo` Right "ab" `withRemainder` "c"

      "" `shouldParseTo` Left (ParseError {reason = "Expected 'a', but got EOF.", currentInput = ""})
      "acd" `shouldParseTo` Left (ParseError {reason = "Expected 'b', but got 'c'.", currentInput = "cd"}) `withRemainder` "acd"

  quickCheckParserSatisfies
    (eitherParse $ charEq 'a')
    "eitherParse never fails"
    (\_ x -> isRight x)
