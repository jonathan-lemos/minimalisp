module Minimalisp.Control.Parser.Text.CharAnySpec where

import Test.Hspec
import Harness.ParserCase
import Minimalisp.Control.Parser.Text.CharAny

spec :: Spec
spec = do
  parserCase
    (charAny "abc")
    "charAny \"abc\""
    $ do
      "a" `shouldParseTo` 'a'
      "bcd" `shouldParseTo` 'b' `withRemainder` "cd"
      "d" `shouldFailWithReason` "Expected any of \"abc\", but got 'd'." `andRemainder` "d"
      "def" `shouldFailWithReason` "Expected any of \"abc\", but got 'd'." `andRemainder` "def"
      "" `shouldFailWithReason` "Expected any of \"abc\", but got EOF." `andRemainder` ""
