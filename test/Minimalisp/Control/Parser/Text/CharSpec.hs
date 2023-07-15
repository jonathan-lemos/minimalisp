module Minimalisp.Control.Parser.Text.CharSpec where

import Test.Hspec
import Harness.ParserCase
import Minimalisp.Control.Parser.Text.Char

spec :: Spec
spec = do
  parserCase
    char
    "char"
    $ do
      "a" `shouldParseTo` 'a'
      "bcd" `shouldParseTo` 'b' `withRemainder` "cd"
      "" `shouldFailWithReason` "Expected any character, but got EOF." `andRemainder` ""
