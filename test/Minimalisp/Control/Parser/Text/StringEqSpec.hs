module Minimalisp.Control.Parser.Text.StringEqSpec where

import Harness.ParserCase
import Test.Hspec
import Minimalisp.Control.Parser.Text.StringEq

spec :: Spec
spec = do
  parserCase
    (stringEq "ab")
    "stringEq \"ab\""
    $ do
      "ab" `shouldParseTo` "ab"
      "abc" `shouldParseTo` "ab" `withRemainder` "c"

      "" `shouldFailWithReason` "Expected 'a', but got EOF." `andRemainder` ""
      "acd" `shouldFailWithReason` "Expected 'b', but got 'c'." `andRemainder` "cd"
