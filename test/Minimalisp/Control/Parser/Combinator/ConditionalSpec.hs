module Minimalisp.Control.Parser.Combinator.ConditionalSpec where

import Harness.ParserCase
import Minimalisp.Control.Parser.Combinator.Conditional
import Minimalisp.Control.Parser.Text.Char
import Test.Hspec

spec :: Spec
spec = do
  parserCase
    (conditional (== 'a') char)
    "conditional (== 'a') char"
    $ do
      "a" `shouldParseTo` 'a'
      "abc" `shouldParseTo` 'a' `withRemainder` "bc"
      "bc" `shouldFailWithReason` "Condition failed." `andRemainder` "bc"
      "" `shouldFailWithReason` "Expected any character, but got EOF." `andRemainder` ""
