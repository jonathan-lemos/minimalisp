module Minimalisp.Control.Parser.Combinator.IgnoreWhitespaceSpec where

import Harness.ParserCase
import Minimalisp.Control.Parser.Combinator.IgnoreWhitespace
import Minimalisp.Control.Parser.Text.Char
import Test.Hspec

spec :: Spec
spec = do
  parserCase
    (ignoreWhitespace char)
    "ignoreWhitespace char"
    $ do
      "a" `shouldParseTo` 'a'
      "abc" `shouldParseTo` 'a' `withRemainder` "bc"
      " a" `shouldParseTo` 'a'
      " abc" `shouldParseTo` 'a' `withRemainder` "bc"
      "" `shouldFailWithReason` "Expected any character, but got EOF." `andRemainder` ""
      " " `shouldFailWithReason` "Expected any character, but got EOF." `andRemainder` ""
